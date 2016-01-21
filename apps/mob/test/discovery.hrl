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
    ServiceName = my_service,

    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> {found, Node} end),
    Result = discovery:where_deployed(FakePeer, ServiceName),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, ServiceName])),
    ?assertEqual({found, Node}, Result),

    meck:validate(peer),
    meck:unload(peer).

should_announce_a_deployed_service_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = self(),
    FakeNode = node(),
    Service = #service{name = 'my_service', command = "a command"},
    Services = sets:new(),
    UpdatedServices = sets:add_element(Service, Services),

    meck:expect(peer, iterative_store, fun(_Peer, {_Key, _Value}) -> ok end),
    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> {found, Services} end),
    discovery:announce_spawned_service(FakePeer, Service, FakeNode),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, services])),
    ?assertEqual(1, meck:num_calls(peer, iterative_store, [FakePeer, {services, UpdatedServices}])),
    ?assertEqual(1, meck:num_calls(peer, iterative_store, [FakePeer, {Service#service.name, FakeNode}])),

    meck:validate(peer),
    meck:unload(peer).

should_init_the_information_on_the_network_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = self(),
    FakeNode = self(),
    EmptyServices = sets:new(),
    Nodes = sets:add_element(FakeNode, sets:new()),
    Providers = ['bash'],

    meck:expect(peer, iterative_store, fun(_Peer, {_Key, _Value}) -> ok end),

    discovery:init_net(FakePeer, FakeNode, Providers),

    ?assertEqual(1, meck:num_calls(peer, iterative_store, [FakePeer, {services, EmptyServices}])),
    ?assertEqual(1, meck:num_calls(peer, iterative_store, [FakePeer, {bash, Nodes}])),
    meck:validate(peer),
    meck:unload(peer).
