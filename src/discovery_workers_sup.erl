-module(discovery_workers_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_worker/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_worker(Host) -> {ok, pid()} when
      Host :: binary().
start_worker(Host) ->
    supervisor:start_child(?MODULE, [Host]).

init([]) ->
	ChildSpec = {
        discovery_worker, {discovery_worker, start_link, []}, temporary,
        5000, worker, []
    },
	{ok, {{simple_one_for_one, 1, 5}, [ChildSpec]}}.
