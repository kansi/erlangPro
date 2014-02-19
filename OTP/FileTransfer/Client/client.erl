-module(client).
-export([write/1]).

-define(SERVER, serverNode@localhost).
-define(CHUNK, 1024*64).

write(Path) ->
    true = net_kernel:connect_node(?SERVER),

    %% send chunks to the server
    FileName = filename:basename(Path),
    {ok, Fdr} = file:open(Path, [read]),

    rpc:call(?SERVER, readWriteServer, write, [{dir, FileName}]),
    read_file(Fdr),
    file:close(Fdr),
    rpc:call(?SERVER, readWriteServer, write, [finish]),

    %% read back the chunks from the server
    {size, TotChunks} = rpc:call(?SERVER,
                             readWriteServer, read, [{self(),FileName}]),

    Range = lists:seq(1,TotChunks),
    ReceiveChunk = fun(Fd) ->
                           receive
                               Msg -> file:write(Fd, Msg)
                           end
                   end,

    DIR = "./output/",
    filelib:ensure_dir(DIR),
    {ok, Fdw} = file:open(DIR ++ FileName, [write]),
    [ ReceiveChunk(Fdw) || _ <- Range ],
    file:close(Fdw),

    ok.

%% read the file and send 64kB chucks to the server
read_file(Fd) ->
    case file:read(Fd, ?CHUNK) of
        {ok, Data} ->
            rpc:call(?SERVER, readWriteServer, write, [Data]),
            read_file(Fd);
        eof ->
            ok
    end.
