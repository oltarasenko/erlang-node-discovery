-module(erlang_node_discovery_tests).

-include_lib("eunit/include/eunit.hrl").


manager_test_() ->
    {foreach,
    fun() ->
        application:load(erlang_node_discovery),
        Modules = [erlang_node_discovery_worker_sup, erlang_node_discovery_db],
        _ = [begin {ok, P} = Mod:start_link(), unlink(P) end || Mod <- Modules],
        Modules
    end,
    fun(Modules) ->
        AllModules = [erlang_node_discovery_manager | Modules],
        _ = [
            begin
                case whereis(Mod) of
                    undefined ->
                        ok;
                    P ->
                        erlang:exit(P, kill),
                        erlang:monitor(process, P),
                        receive {'DOWN', _, _, P, _} -> ok end
                end
            end || Mod <- AllModules
        ],
        _ = [application:unset_env(erlang_node_discovery, Key) || Key <- [node_names, hosts, db_callback]],
        ok
    end,
    [
        {"starting with static nodes, checking info", fun() ->
            NodePorts= [{"a", 11}, {<<"b">>, 12}, {c, 13}],
            Hosts = ["host1", <<"host2">>, host3],
            application:set_env(erlang_node_discovery, node_ports, NodePorts),
            application:set_env(erlang_node_discovery, hosts, Hosts),
            Pid = start_manager(),
            Info = erlang_node_discovery_manager:get_info(),
            Workers = lists:sort(proplists:get_value(workers, Info, [])),
            Callback = proplists:get_value(db_callback, Info),
            ?assertEqual(
                [
                    'a@host1', 'a@host2', 'a@host3',
                    'b@host1', 'b@host2', 'b@host3',
                    'c@host1', 'c@host2', 'c@host3'
                ],
                [Node || {Node, _Pid} <- Workers]
            ),
            ?assertEqual(
                erlang_node_discovery_db,
                Callback
            ),
            ?assertEqual(
                [
                    {'a@host1', 11}, {'a@host2', 11}, {'a@host3', 11},
                    {'b@host1', 12}, {'b@host2', 12}, {'b@host3', 12},
                    {'c@host1', 13}, {'c@host2', 13}, {'c@host3', 13}
                ],
                lists:sort(erlang_node_discovery_manager:list_nodes())
            )
        end},
        {"adding dynamic nodes", fun() ->
            Pid = start_manager(),
            ?assertEqual(
                [],
                proplists:get_value(workers, erlang_node_discovery_manager:get_info(), [])
            ),
            ?assertEqual(ok, erlang_node_discovery_manager:add_node('some@node', some_port)),
            ?assertMatch(
                [{'some@node', _}],
                proplists:get_value(workers, erlang_node_discovery_manager:get_info(), [])
            )
        end},
        {"removing dynamic nodes", fun() ->
            Pid = start_manager(),
            ?assertEqual(
                [],
                proplists:get_value(workers, erlang_node_discovery_manager:get_info(), [])
            ),
            erlang_node_discovery_manager:add_node('some@node', some_port),
            erlang_node_discovery_manager:remove_node('some@node'),
            ?assertEqual(
                [],
                proplists:get_value(workers, erlang_node_discovery_manager:get_info(), [])
            )
        end},
        {"worker should be restared if it's crashed, manager performs supervisor role here"
         "just to maintain proper state (node -> pid)", fun() ->
            Pid = start_manager(),
            ?assertEqual(
                [],
                proplists:get_value(workers, erlang_node_discovery_manager:get_info(), [])
            ),
            erlang_node_discovery_manager:add_node('some@node', some_port),
            [{'some@node', WPid}] = proplists:get_value(workers, erlang_node_discovery_manager:get_info(), []),
            erlang:exit(WPid, kill),
            erlang:monitor(process, WPid),
            receive {'DOWN', _, _, _, _} -> ok end,
            [{'some@node', NewWPid}] = proplists:get_value(workers, erlang_node_discovery_manager:get_info(), []),
            ?assert(WPid /= NewWPid)
        end}
    ]}.



start_manager() ->
    {ok, P} = erlang_node_discovery_manager:start_link(),
    unlink(P),
    P.
