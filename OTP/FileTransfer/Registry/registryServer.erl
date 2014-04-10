-module(registryServer).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, registerServer/1, getFileServers/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {servers}).

%%%===================================================================
%%% API function for registering and querying server
%%%===================================================================

registerServer({register, ServerName, Pid}) ->
    gen_server:call(?MODULE, {register, ServerName, Pid});

registerServer({unregister, ServerName, Pid}) ->
    gen_server:call(?MODULE, {unregister, ServerName, Pid}).

getFileServers() -> gen_server:call(?MODULE, {getInfo}).

%% start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{servers=[]}}.

%% Register the server
handle_call({register, ServerName, Pid}, _From, State) ->

    %% Add the new server as master or worker
    NewServerList = case State#state.servers of
                   [] ->
                       lists:append(State#state.servers, [{master,ServerName,
                                                           Pid}]);
                   _   ->
                       lists:append(State#state.servers, [{worker,ServerName,
                                                           Pid}])
            end,

    NewState = #state{servers=NewServerList},
    %io:format("~p~n" , [NewState#state.servers]),
    %% prepare and send reply to the client
    Reply = ok,
    {reply, Reply, NewState};

%% remove the Server from the list
handle_call({unregister, ServerName, _Pid}, _From, State) ->
    OldList = State#state.servers,
    NewList = lists:keydelete(ServerName, 2, OldList),

    io:format("NewList : ~p~n", [NewList]),
    Reply = ok,
    {reply, Reply, #state{servers = NewList}};

%% send server info to the requesting client
handle_call({getInfo}, _From, State) ->
    Reply = State#state.servers,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%% useless function
handle_cast(_Msg, State) ->
    {noreply, State}.

%% useless function
handle_info(_Info, State) ->
    {noreply, State}.

%% stop the server
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
