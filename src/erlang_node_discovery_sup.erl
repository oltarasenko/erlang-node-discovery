-module(erlang_node_discovery_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WorkersSup = {
        erlang_node_discovery_worker_sup,
        {erlang_node_discovery_worker_sup, start_link, []},
        permanent,
        5000,
        supervisor,
        [erlang_node_discovery_worker_sup]
    },
    WorkersManager =  {
        erlang_node_discovery_manager, {erlang_node_discovery_manager, start_link, []}, permanent,
        5000, worker, [erlang_node_discovery_manager]
    },
	{ok, {{one_for_one, 4, 3600}, [WorkersSup, WorkersManager]}}.
