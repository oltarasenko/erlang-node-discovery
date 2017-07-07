-module(erlang_node_discovery_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	erlang_node_discovery_sup:start_link().

stop(_State) ->
	ok.
