-module(erlang_node_discovery_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_worker/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_worker(Node) -> {ok, pid()} when
      Node :: atom().
start_worker(Node) ->
    error_logger:info_msg("DBG: ~p", ["Starting worker ~n"]),
    supervisor:start_child(?MODULE, [Node]).


init([]) ->
	ChildSpec = {
        erlang_node_discovery_worker, {erlang_node_discovery_worker, start_link, []}, temporary,
        5000, worker, []
    },
	{ok, {{simple_one_for_one, 1, 5}, [ChildSpec]}}.


