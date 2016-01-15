-include_lib("eunit/include/eunit.hrl").

should_try_to_find_an_available_node_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = fake_peer,
    FakeNode = fake_node,
    Service = #service{name = my_service, command = "a command"},

    StoredNodes = sets:from_list([FakeNode, FakeNode]),
    meck:expect(peer, iterative_find_value, fun(_, _) -> {found, StoredNodes} end),
    {ok, Node} = discovery:find_available_node(FakePeer, Service),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, nodes])),
    ?assertEqual(FakeNode, Node),

    meck:validate(peer),
    meck:unload(peer).

should_return_an_error_if_no_nodes_are_found_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = fake_peer,
    FakeNode = fake_node,
    Service = #service{name = my_service, command = "a command"},

    StoredNodes = sets:from_list([FakeNode, FakeNode]),
    meck:expect(peer, iterative_find_value, fun(_, _) -> StoredNodes end),
    {error, Error} = discovery:find_available_node(FakePeer, Service),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, nodes])),
    ?assertEqual(Error, no_nodes),

    meck:validate(peer),
    meck:unload(peer).

should_find_where_is_deployed_a_service_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = self(),
    Node = node(),
    Service = #service{name = my_service, command = "a command"},

    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> {found, Node} end),
    Result = discovery:where_deployed(FakePeer, Service),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, Service#service.name])),
    ?assertEqual({found, Node}, Result).
