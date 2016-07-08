-include_lib("eunit/include/eunit.hrl").

should_ask_if_a_service_is_remotely_started_test() ->
    start_mocks([remote_mob, mob_dht]),
    FakeNode = fake_node,

    meck:expect(mob_dht, where_deployed, fun(_) -> {found, FakeNode} end),
    meck:expect(remote_mob, is_started, fun(_, _) -> true end),

    ServiceName = my_service,
    Result = mob_router:is_started(my_service),

    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [ServiceName])),
    ?assertEqual(1, meck:num_calls(remote_mob, is_started, [FakeNode, ServiceName])),
    ?assert(Result),
    stop_mocks([remote_mob, mob_dht]).

should_add_a_child_a_to_a_remotely_spawned_parent_test() ->
    start_mocks([mob_dht, remote_mob]),
    FakeNode = self(),

    meck:expect(mob_dht, where_deployed, fun(_ServiceName) -> {found, FakeNode} end),
    meck:expect(remote_mob, add_child, fun(_Node, _Parent, _Child) -> ok end),

    Parent = parent,
    Child = child,
    mob_router:add_child(Parent, Child),
    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [Parent])),
    ?assertEqual(1, meck:num_calls(remote_mob, add_child, [FakeNode, Parent, Child])),
    stop_mocks([mob_dht, remote_mob]).

start_mocks([]) -> ok;
start_mocks([Mod | Modules]) ->
    meck:new(Mod, [non_strict]),
    start_mocks(Modules).

stop_mocks([]) -> ok;
stop_mocks([Mod | Modules]) ->
    ?assert(meck:validate(Mod)),
    meck:unload(Mod),
    stop_mocks(Modules).
