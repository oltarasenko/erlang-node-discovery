-module(erlang_node_discovery_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([add_node/3]).
-export([remove_node/1]).
-export([list_nodes/0]).
-export([get_info/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    db_callback  :: module(), %% add_node/3, remove_node/1, list_nodes/0
    resolve_func :: fun((term()) -> term()),
    workers      :: map()     %% node -> worker_pid()
}).


-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec add_node(node(), inet:hostname(), inet:port_number()) -> ok.
add_node(Node, Host, Port) ->
    gen_server:call(?MODULE, {add_node, Node, Host, Port}, infinity).


-spec remove_node(node()) -> ok.
remove_node(Node) ->
    gen_server:call(?MODULE, {remove_node, Node}, infinity).


-spec list_nodes() -> [{node(), inet:hostname(), inet:port_number()}].
list_nodes() ->
    gen_server:call(?MODULE, list_nodes, infinity).


-spec get_info() -> Info when
      Info     :: [InfoElem],
      InfoElem :: {workers, [{node(), pid()}]}
                | {db_callback, module()}.
get_info() ->
    gen_server:call(?MODULE, get_info, infinity).


%% gen_server.

init([]) ->
    Callback = application:get_env(erlang_node_discovery, db_callback, erlang_node_discovery_db),
    error_logger:info_msg("Using ~p as node db~n", [Callback]),
    case application:get_env(erlang_node_discovery, cookie) of
        undefined -> ok;
        {ok, Cookie} -> erlang:set_cookie(node(), Cookie)
    end,
    ResolveFunc = case application:get_env(erlang_node_discovery, resolve_func) of
        undefined -> fun(H) -> H end;
        {ok, F} when is_function(F, 1) -> F;
        {ok, {M, F}} -> fun M:F/1
    end,
    Hosts = application:get_env(erlang_node_discovery, hosts, []),
    NodePorts = application:get_env(erlang_node_discovery, node_ports, []),
    %% adding static nodes to db
    _ = [
        begin
            Node = list_to_atom(lists:flatten(io_lib:format("~s@~s", [NodeName, Host]))),
            Callback:add_node(Node, ResolveFunc(Host), Port),
            error_logger:info_msg("Added static node to db ~s~n", [Node])
        end || {NodeName, Port} <- NodePorts, Host <- Hosts
    ],
	{ok, reinit_workers(#state{workers = #{}, resolve_func = ResolveFunc, db_callback = Callback})}.


handle_call({add_node, Node, Host, Port}, _From, State = #state{db_callback = Callback, resolve_func = RF}) ->
    ok = Callback:add_node(Node, RF(Host), Port),
    {reply, ok, reinit_workers(State)};

handle_call({remove_node, Node}, _From, State = #state{db_callback = Callback}) ->
    ok = Callback:remove_node(Node),
    {reply, ok, reinit_workers(State)};

handle_call(list_nodes, _From, State = #state{db_callback = Callback}) ->
    {reply, Callback:list_nodes(), State};

handle_call(get_info, _From, State = #state{workers = Workers, db_callback = Callback}) ->
    Info = [
        {workers, maps:to_list(Workers)},
        {db_callback, Callback}
    ],
    {reply, Info, State};

handle_call(Msg, _From, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
	{reply, {error, {bad_msg, Msg}}, State}.


handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.


handle_info({'DOWN', _Ref, _Type, Pid, Reason}, State) ->
    error_logger:info_msg("Worker down with reason: ~p~n", [Reason]),
    {noreply, reinit_workers(worker_down(State, Pid))};

handle_info(Msg, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% internal funcs


-spec worker_down(State, Pid) -> NewState when
      State    :: #state{},
      Pid      :: pid(),
      NewState :: #state{}.
worker_down(State = #state{workers = Workers}, Pid) ->
    FoldFun = fun(Node, P, Acc) ->
        case P == Pid of
            true -> Acc;
            false -> Acc#{Node => P}
        end
    end,
    State#state{workers = maps:fold(FoldFun, #{}, Workers)}.


-spec reinit_workers(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
reinit_workers(State) ->
    remove_workers(add_workers(State)).


-spec add_workers(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
add_workers(State = #state{db_callback = Callback, workers = Workers}) ->
    FoldlFun = fun({Node, {_Host, _Port}}, TmpWorkers) ->
        case maps:find(Node, TmpWorkers) of
            error ->
                {ok, Pid} = erlang_node_discovery_worker_sup:start_worker(Node),
                erlang:monitor(process, Pid),
                error_logger:info_msg("Started discovery worker for node ~s at ~p~n", [Node, Pid]),
                TmpWorkers#{Node => Pid};
            {ok, _} ->
                TmpWorkers
        end
    end,
    State#state{workers = lists:foldl(FoldlFun, Workers, Callback:list_nodes())}.


-spec remove_workers(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
remove_workers(State = #state{db_callback = Callback, workers = Workers}) ->
    Nodes = [Node || {Node, {_Host, _Port}} <- Callback:list_nodes()],
    FoldFun = fun(Node, Pid, TmpWorkers) ->
        case lists:member(Node, Nodes) of
            true ->
                TmpWorkers#{Node => Pid};
            false ->
                ok = erlang_node_discovery_worker_sup:stop_worker(Pid),
                error_logger:info_msg("Stopped discovery worker for node ~s at ~p~n", [Node, Pid]),
                TmpWorkers
        end
    end,
    State#state{workers = maps:fold(FoldFun, #{}, Workers)}.
