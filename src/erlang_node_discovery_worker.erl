-module(erlang_node_discovery_worker).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
    node    :: node(),
    node_up :: boolean(),
    timer   :: reference()
}).


-spec start_link(node()) -> {ok, pid()}.
start_link(Node) ->
	gen_server:start_link(?MODULE, Node, []).


init(Node) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, init_timer(#state{node = Node, node_up = false}, 0)}.


handle_call(Msg, _From, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
	{reply, {error, {bad_msg, Msg}}, State}.


handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.


handle_info({nodeup, Node}, State = #state{node = Node}) ->
    {noreply, State#state{node_up = true}};

handle_info({nodeup, _}, State) ->
    {noreply, State};

handle_info({nodedown, Node}, State = #state{node = Node}) ->
    {noreply, init_timer(State#state{node_up = false})};

handle_info({nodedown, _}, State) ->
    {noreply, State};

handle_info(ping, State = #state{node_up = true}) ->
    {noreply, State};
handle_info(ping, State = #state{node = Node}) ->
    _ = net_adm:ping(Node),
    {noreply, init_timer(State)};

handle_info(Msg, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% internal funcs


-spec init_timer(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
init_timer(State) ->
    init_timer(State, 30000).


-spec init_timer(State, Delay) -> NewState when
      State    :: #state{},
      Delay    :: non_neg_integer(),
      NewState :: #state{}.
init_timer(State = #state{timer = Timer}, Delay) ->
    catch erlang:cancel_timer(Timer),
    State#state{timer = erlang:send_after(Delay, self(), ping)}.

