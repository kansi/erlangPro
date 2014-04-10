-module(client).
-export([write/3, read/3, clnttcp_create/1, chunkDispacher/1]).

-define(SERVER_NODE, serverNode@localhost).
-define(REGISTRY_SERVER, registry@localhost).
-define(DIR, "./output/").
-define(TMPDIR, "./tmp/").
-define(SVR_MODULE, fileServer).
-define(TIMEOUT, 60000).
-define(CHUNK, 1024*64).
-define(CHUNKNO, 1).

%% create a reader writer object for the client
clnttcp_create({write, Path}) ->

    %% get list of servers from registry server
    ServerList = rpc:call(?REGISTRY_SERVER, registryServer, getFileServers,
                          []),

    io:format("~p~n", [ServerList]),

    %% get the name of the master server
    MasterServer = [Name || {master, Name, _Pid} <- ServerList],

    %% spawn a client thread that sends write request to the server.
    spawn(?MODULE, write, [?SERVER_NODE, hd(MasterServer), Path]),
    ok;

%% create a reader object for the client
clnttcp_create({read, FileName}) ->

    %% get list of servers from registry server
    ServerList = rpc:call(?REGISTRY_SERVER, registryServer, getFileServers,
                          []),

    %% get the name of master server
    MasterServer = [Name  || {master, Name, _Pid} <- ServerList],

    %% get the name of all servers
    ServerNames  = [SName || {_, SName, _} <- ServerList],
    io:format("~nServers : ~p~n", [ServerNames]),

    %% get the total Number of chunks from the master server.
    {ok, TotChunks} = rpc:call(?SERVER_NODE, ?SVR_MODULE,
                               getTotalNumberOfChunks,
                               [{hd(MasterServer), FileName}]),

    %% spawn a "chunkDispacher" thread that will give chunk numbers to the
    %% client threads to fetch them
    Dispacher = spawn(?MODULE, chunkDispacher, [lists:seq(1,TotChunks)]),

    %% spawn the client threads that will get chunk number from the dispacher
    %% and fetch from the server.
    spawn_readers(?SERVER_NODE, ServerNames, {FileName, 0, Dispacher}),

    %% join all the recieved chunks
    join_chunks(?DIR, ?TMPDIR, FileName),
    ok.

%% function : client spawns this function to write data to the master server
write(NodeName, ServerName, Path) ->

    %% Get the fileName from the path
    FileName  = filename:basename(Path),
    %% open file
    {ok, Fdr} = file:open(Path, [read]),

    %% read 64Kb chunks and send it to the server
    read_file(NodeName, ServerName, Fdr, FileName, ?CHUNKNO),
    %% close file descriplter
    file:close(Fdr),
    ok.

%% function : Gives out chunk number to client threads. The client threads
%% request chunk no. from the "chunkDispacher".
chunkDispacher(ChunkList) ->
    %% wait for the threads to ask for chunk number
    receive
        {From, getChunkNo} ->
            if
                %% if the chunk list is empty then send "finish" msg to thread
                ChunkList == [] ->
                    From ! {finish},
                    chunkDispacher(ChunkList);

                %% send the ChunkNo to client thread
                true ->
                    From ! {chunkNo, hd(ChunkList)},
                    chunkDispacher(tl(ChunkList))
            end;

        %% if chunk couldn't be requested by client thread then "ChunkNo" back to
        %% the "ChunkList" list
        {error, ChunkNo} ->
            chunkDispacher(ChunkList ++ [ChunkNo])
    after
        ?TIMEOUT -> erlang:exit("[+] DISPACHER EXITING")
    end.

%% function : spawns client thread that execute the "read" function.
spawn_readers(NodeName, ServerList,
              {FileName, TotClinets, Dispacher}) ->

    %% spawn N threads, where N is the number of File Servers.
    if
        TotClinets < erlang:length(ServerList) ->

            %% get the name of the server
            [Server|T] = ServerList,

            %% spawn a new client to read new chunk
            spawn_monitor(?MODULE, read, [NodeName, ServerList, {FileName,
                                                             Dispacher}]),
            spawn_readers(NodeName, T ++ [Server], {FileName, TotClinets+1,
                                                    Dispacher});
        true ->
            %% wait till all the client threads exit
            exit_readers(TotClinets),
            io:format("All Readers DONE !~n")
    end.

%% function : waits till all client threads exit
exit_readers(TotClinets) ->
    if
        TotClinets > 0 ->
            receive
                _Msg ->
                    exit_readers(TotClinets-1)
            after
                ?TIMEOUT -> ok
            end;

        true -> ok
    end.

%% function : each client thread executes this function. This function get
%% ChunkNo from the "dispacher" and then requests for that chunk from the
%% server.
read(NodeName, ServerList, {FileName, Dispacher}) ->

    %% get the chunk number from chunkDispacher to read from the server
    Dispacher ! {self(), getChunkNo},

    %% recieve chunk number from the dispacher
    receive
        {chunkNo, ChunkNo} ->

            %% get the name of the server to fetch the chunk from
            ServerName = hd(ServerList),

            %% read back the chunks from the server. Timeout occurs if the
            %% server doesnt respond and the thread informs the "dispacher" that
            %% chunk coundnt be fetched.
            Data = case rpc:call(NodeName, ?SVR_MODULE, read,
                                 [{ServerName, self(),FileName, ChunkNo}], 2000)
                   of
                       {ok, Chunk} ->
                           Chunk;
                       _   ->
                           %% inform the dispacher about failure in chunk
                           %% fetching
                           Dispacher ! {error, ChunkNo},
                           io:format("~n[-] Exiting(pid:~p): ~p not responding~n~n",
                                     [self(),ServerName]),
                           erlang:exit("Client Exiting")
                   end,

            io:format("[+] Client thread(pid:~p) received ChunkNo : ~p~n", [self(),ChunkNo]),

            %% Write the recieved chuck into the temp folder
            Dir = ?TMPDIR,
            ok = filelib:ensure_dir(Dir),
            FilePath = Dir ++ "chunk_" ++
                                        erlang:integer_to_list(ChunkNo),
            file:write_file(FilePath, Data),

            read(NodeName, ServerList, {FileName, Dispacher});

        %% no more chunks to fetch, so the thread exits.
        {finish} ->
            io:format("[+] Client thread(pid:~p) exiting~n", [self()]),
            ok
    after
        ?TIMEOUT -> erlang:exit("[-] NO REPLY FROM DISPACHER")
    end.

%%===================================================
%% HELPER Functions
%%===================================================
%%
%% function : read the local file and send 64kB chucks to the server
read_file(NodeName, ServerName, Fd, FileName, ChunkNo) ->
    case file:read(Fd, ?CHUNK) of
        {ok, Data} ->
            rpc:call(NodeName, ?SVR_MODULE, write,
                     [{ServerName, FileName, ChunkNo, Data}]),
            read_file(NodeName, ServerName, Fd, FileName, ChunkNo+1);
        eof ->
            {ok, ChunkNo}
    end.

%% function : join the recieved chunks in the "output" folder
join_chunks(OutDir, InDir, FileName) ->
    ok          = filelib:ensure_dir(OutDir),
    {ok, Files} = file:list_dir(InDir),

    {ok, Fd}    = file:open(OutDir ++ FileName, [write]),
    AppendFile  = fun(Fdr, DIR, ChunkName) ->
                         {ok, BinData} = file:read_file(DIR ++ ChunkName),
                         file:write(Fdr, erlang:binary_to_list(BinData))
                 end,

    [AppendFile(Fd, InDir, ChunkName) || ChunkName <- lists:sort(Files)],
    os:cmd("rm -rf " ++ InDir),
    file:close(Fd).
