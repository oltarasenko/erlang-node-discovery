-module(erlang_node_discovery_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    hosts   :: binary(),
    workers :: list()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    Hosts = application:get_env(erlang_node_discovery, hosts, []),
    Nodes = application:get_env(erlang_node_discovery, nodes, []),
    {_Mod, _RegisterCallback} = application:get_env(
        erlang_node_discovery, register_callback
    ),

    Workers = lists:map(
        fun({Host, {NodeName, _Port}}) ->

            HostFullName = list_to_atom(lists:flatten(io_lib:format("~s@~s", [NodeName, Host]))),
            % Mod:RegisterCallback(list_to_atom(HostFullName), Port),
            {ok, Pid} = erlang_node_discovery_worker_sup:start_worker(HostFullName),
            Pid
        end,
        [{Host, Node} || Host <- Hosts, Node <-Nodes]
    ),
	{ok, #state{workers = Workers}}.
    % {ok, #state{}}.


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
