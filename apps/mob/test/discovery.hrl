-include_lib("eunit/include/eunit.hrl").

should_try_to_find_an_available_node_for_a_service_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = fake_peer,
    FakeNode = fake_node,
    Service = #service{name = 'my_service', provider = 'bash', params = #{"command" => "a command"}},

    StoredNodes = sets:from_list([FakeNode, FakeNode]),
    meck:expect(peer, iterative_find_value, fun(_, _) -> {found, StoredNodes} end),
    {ok, Node} = discovery:handle_find_available_node(Service, FakePeer),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, Service#service.provider])),
    ?assertEqual(FakeNode, Node),

    meck:validate(peer),
    meck:unload(peer).

should_return_an_error_if_no_nodes_are_found_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = fake_peer,
    FakeNode = fake_node,
    Service = #service{name = 'my_service', provider = 'bash', params = #{"command" => "a command"}},

    StoredNodes = sets:from_list([FakeNode, FakeNode]),
    meck:expect(peer, iterative_find_value, fun(_, _) -> StoredNodes end),
    {error, Error} = discovery:handle_find_available_node(Service, FakePeer),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, Service#service.provider])),
    ?assertEqual(Error, no_nodes),

    meck:validate(peer),
    meck:unload(peer).

should_find_where_is_deployed_a_service_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = self(),
    Node = node(),
    ServiceName = my_service,

    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> {found, Node} end),
    Result = discovery:handle_where_deployed(ServiceName, FakePeer),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [FakePeer, ServiceName])),
    ?assertEqual({found, Node}, Result),

    meck:validate(peer),
    meck:unload(peer).

should_announce_a_deployed_service_test() ->
    meck:new(peer, [non_strict]),
    FakePeer = self(),
    FakeNode = node(),
    Service = #service{name = 'my_service', provider = 'bash', params = #{"command" => "a command"}},
    Services = sets:new(),
    UpdatedServices = sets:add_element(Service, Services),

    meck:expect(peer, iterative_store, fun(_Peer, {_Key, _Value}) -> ok end),
    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> {found, Services} end),
    discovery:handle_announce_spawned_service(Service, FakePeer),

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
