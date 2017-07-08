-module(erlang_node_discovery_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_worker/1]).
-export([stop_worker/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_worker(node()) -> {ok, pid()}.
start_worker(Node) ->
    supervisor:start_child(?MODULE, [Node]).


-spec stop_worker(pid()) -> ok | {error, not_found}.
stop_worker(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).


init([]) ->
	ChildSpec = {
        erlang_node_discovery_worker, {erlang_node_discovery_worker, start_link, []}, temporary,
        5000, worker, []
    },
	{ok, {{simple_one_for_one, 4, 3600}, [ChildSpec]}}.


