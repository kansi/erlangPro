-module(readWriteServer).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, write/1, read/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {chunk=0, path="."}).

%%%===================================================================
%%% API
%%%===================================================================

write({dir, Name})  -> gen_server:call(?MODULE, {dir, Name});
write(finish)       -> gen_server:call(?MODULE, finish);
write(Data)         -> gen_server:call(?MODULE, {write, Data}).
read({Client, Dir}) -> gen_server:call(?MODULE, {read, Client, Dir}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% Create directory same as the filename
handle_call({dir, Name}, _From, State) ->
    %% create directory with the name of the file
    Path = "./" ++ Name ++ "/",
    filelib:ensure_dir(Path),

    %% prepare reply and new state
    Reply = State#state.chunk,
    NewState = #state{chunk=Reply, path=Path},

    {reply, Reply, NewState};

%% write the chucks recieved into the folder
handle_call({write, Data}, _From, State) ->
    %% increment chunk size
    ChunkCount = State#state.chunk + 1,
    Path       = State#state.path,

    %% write chuck into the folder if size is 64Kb
    Reply = case erlang:length(Data) of
                1024*64 ->
                    FilePath = Path ++ "chunk_" ++
                                    erlang:integer_to_list(ChunkCount),
                    file:write_file(FilePath, Data),
                    ok;
                _   ->
                    "[-] ERROR : Bad chunk size."
            end,

    NewState   = #state{chunk=ChunkCount, path=Path},

    {reply, Reply, NewState};

%% reset state when all the chunks are written
handle_call(finish, _From, _State) ->
    NewState = #state{chunk=0},
    {reply, ok, NewState};

%% read and transmit data back to client
handle_call({read, Client, Dir}, From, State) ->

    case filelib:is_dir(Dir) of
        false ->
            erlang:exit("Bad Filename");
        true ->
            ok
    end,
    {ok, Files} = file:list_dir(Dir),

    %% declare the number of chunks being sent
    gen_server:reply(From, {size, erlang:length(Files)}),

    %% send the chunks to the client
    ExtractData = fun(To, FileName) ->
                         {ok, BinData} = file:read_file(FileName),
                         Reply = erlang:binary_to_list(BinData),
                         To ! Reply
                 end,

    [ExtractData(Client, Dir ++ "/" ++ X ) || X <- lists:sort(Files)],
    Reply = ok,

    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
