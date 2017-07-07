-module(erlang_node_discovery_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WorkersManager =  {discovery_workers_manager, {discovery_workers_manager, start_link, []}, permanent,
                5000, worker, [discovery_workers_manager]},
    WorkersSup  = {discovery_workers_sup, {discovery_workers_sup, start_link, []}, permanent,
               brutal_kill, supervisor, [discovery_workers_sup]},
	{ok, {{one_for_one, 1, 5}, [WorkersManager, WorkersSup]}}.
