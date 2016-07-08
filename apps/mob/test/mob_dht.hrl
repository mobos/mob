-include_lib("eunit/include/eunit.hrl").

-define(SERVICE, #service{name = 'my_service', provider = 'bash', params = #{}}).
-define(SECOND_SERVICE, #service{name = 'my_second_service', provider = 'bash', params = #{}}).
-define(NODES_PROVIDING_BASH, sets:from_list(['0000@fakenode', '0000@fakenode'])).
-define(FAKE_PEER, {self(), 0}).
-define(FAKE_NODE, '0000@fakenode').

finds_an_available_node_for_a_service_test() ->
    meck:new(peer, [non_strict]),

    meck:expect(peer, iterative_find_value, fun(_, _) -> {found, ?NODES_PROVIDING_BASH} end),

    {ok, Node} = mob_dht:handle_find_available_node(?SERVICE, ?FAKE_PEER),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [?FAKE_PEER, ?SERVICE#service.provider])),
    ?assertEqual(?FAKE_NODE, Node),

    meck:validate(peer),
    meck:unload(peer).

returns_an_error_if_no_nodes_are_found_for_a_service_test() ->
    meck:new(peer, [non_strict]),

    meck:expect(peer, iterative_find_value, fun(_, _) -> [?FAKE_PEER] end),
    {error, Error} = mob_dht:handle_find_available_node(?SERVICE, ?FAKE_PEER),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [?FAKE_PEER, ?SERVICE#service.provider])),
    ?assertEqual(Error, no_nodes),

    meck:validate(peer),
    meck:unload(peer).

finds_where_is_deployed_a_service_test() ->
    meck:new(peer, [non_strict]),

    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> {found, ?FAKE_NODE} end),

    {found, Node} = mob_dht:handle_where_deployed(?SERVICE#service.name, ?FAKE_PEER),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [?FAKE_PEER, ?SERVICE#service.name])),
    ?assertEqual(?FAKE_NODE, Node),

    meck:validate(peer),
    meck:unload(peer).

returns_an_error_if_a_service_is_not_deployed_test() ->
    meck:new(peer, [non_strict]),

    meck:expect(peer, iterative_find_value, fun(_Peer, _Key) -> [?FAKE_PEER] end),

    {error, Error} = mob_dht:handle_where_deployed(?SERVICE#service.name, ?FAKE_PEER),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [?FAKE_PEER, ?SERVICE#service.name])),
    ?assertEqual(not_found, Error),

    meck:validate(peer),
    meck:unload(peer).

announce_a_deployed_service_test() ->
    meck:new(peer, [non_strict]),

    Services = sets:from_list([?SERVICE]),
    UpdatedServices = sets:from_list([?SERVICE, ?SECOND_SERVICE]),

    meck:expect(peer, iterative_store, fun(_, _) -> ok end),
    meck:expect(peer, iterative_find_value, fun(_, _) -> {found, Services} end),

    ok = mob_dht:handle_announce_spawned_service(?SECOND_SERVICE, ?FAKE_NODE, ?FAKE_PEER),

    ?assertEqual(1, meck:num_calls(peer, iterative_find_value, [?FAKE_PEER, services])),
    ?assertEqual(1, meck:num_calls(peer, iterative_store, [?FAKE_PEER, {services, UpdatedServices}])),
    ?assertEqual(1, meck:num_calls(peer, iterative_store, [?FAKE_PEER, {?SECOND_SERVICE#service.name, ?FAKE_NODE}])),

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

    mob_dht:init_net(FakePeer, FakeNode, Providers),

    ?assertEqual(1, meck:num_calls(peer, iterative_store, [FakePeer, {services, EmptyServices}])),
    ?assertEqual(1, meck:num_calls(peer, iterative_store, [FakePeer, {bash, Nodes}])),
    meck:validate(peer),
    meck:unload(peer).
