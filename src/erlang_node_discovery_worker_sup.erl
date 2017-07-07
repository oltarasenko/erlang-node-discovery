-module(erlang_node_discovery_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_worker/2]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_worker(Host, Port) -> {ok, pid()} when
      Host :: binary(),
      Port :: pos_integer().
start_worker(Host, Port) ->
    supervisor:start_child(?MODULE, [{Host, Port}]).


init([]) ->
	ChildSpec = {
        discovery_worker, {discovery_worker, start_link, [fun register/2]}, temporary,
        5000, worker, []
    },
	{ok, {{simple_one_for_one, 1, 5}, [ChildSpec]}}.


