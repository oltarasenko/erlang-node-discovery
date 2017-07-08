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
    node :: atom()
}).

-define(PING_TIMER, 5000).
%% API.

-spec start_link(Node) -> Result when
    Node :: atom(),
    Result :: {ok, pid()}.
start_link(Node) ->
    error_logger:info_msg("DBG: ~p", ["Start link called"]),
	gen_server:start_link(?MODULE, Node, []).

%% gen_server.

init(Node) ->
    error_logger:info_msg("DBG: ~p", ["NODE PASSED"]),
    net_kernel:monitor_nodes(true),
    {ok, #state{node = Node}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({nodedown, Node}, State = #state{node = Node}) ->
    erlang:send_after(?PING_TIMER, self(), connect),
    {noreply, State};
handle_info(connect, State = #state{node = Node}) ->
    case net_adm:ping(Node) of
        pong -> ok;
        pang -> erlang:send_after(?PING_TIMER, self(), connect)
    end,
    {noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
