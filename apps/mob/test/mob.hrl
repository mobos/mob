-include_lib("eunit/include/eunit.hrl").

should_find_the_correct_node_and_deploy_a_service_test() ->
    start_mocks([service_parser, mob_dht, remote_mob]),

    Service = "{
                 \"name\": \"my_service\",
                 \"provider\": \"bash\",
                 \"params\": {
                     \"command\": \"a command\"
                 }
               }",

    FakeNode = fake_node,
    ParsedService = #service{name = 'my_service', provider = 'bash', params = #{"command" => "a command"}},

    meck:expect(service_parser, parse, fun(_) -> {ok, ParsedService} end),
    meck:expect(mob_dht, where_deployed, fun(_) -> {error, not_found} end),
    meck:expect(mob_dht, find_available_node, fun(_) -> {ok, FakeNode} end),
    meck:expect(remote_mob, run, fun(_, _) -> ok end),

    State = #state{},

    {Reply, NewState} = mob:handle_deploy(Service, State),

    ?assertEqual(1, meck:num_calls(service_parser, parse, [Service])),
    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [ParsedService#service.name])),
    ?assertEqual(1, meck:num_calls(mob_dht, find_available_node, [ParsedService])),
    ?assertEqual(1, meck:num_calls(remote_mob, run, [FakeNode, ParsedService])),
    ?assertEqual(State, NewState),
    ?assertEqual(FakeNode, Reply),

    stop_mocks([service_parser, mob_dht, remote_mob]).


should_not_deploy_an_already_deployed_service_test() ->
    start_mocks([service_parser, mob_dht]),

    Service = "{
                 \"name\": \"my_service\",
                 \"provider\": \"bash\",
                 \"params\": {
                     \"command\": \"a command\"
                 }
               }",

    FakePeer = self(),
    FakeNode = fake_node,

    ParsedService = #service{name = 'my_service', provider = 'bash', params = #{"command" => "a command"}},

    meck:expect(service_parser, parse, fun(_) -> {ok, ParsedService} end),
    meck:expect(mob_dht, where_deployed, fun(_) -> {found, FakeNode} end),

    State = #state{},
    {Reply, NewState} = mob:handle_deploy(Service, State),

    ?assertEqual(1, meck:num_calls(service_parser, parse, [Service])),
    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [ParsedService#service.name])),
    ?assertEqual(State, NewState),
    ?assertEqual('already_deployed', Reply),

    stop_mocks([service_parser, mob_dht]).

should_reply_with_an_error_message_if_the_service_isnt_correct_test() ->
    meck:new(service, [non_strict]),
    WrongService = "[ a wrong service }",
    FakePeer = self(),

    State = #state{},
    ErrorMessage = format_error,

    meck:expect(service, parse, fun(_Service) -> {error, ErrorMessage} end),
    {Reply, NewState} = mob:handle_deploy(WrongService, State),

    ?assertEqual(ErrorMessage, Reply),
    ?assertEqual(State, NewState),

    stop_mocks([service]).

should_ask_if_a_service_is_remotely_started_test() ->
    start_mocks([remote_mob, mob_dht]),
    FakeNode = fake_node,

    meck:expect(mob_dht, where_deployed, fun(_) -> {found, FakeNode} end),
    meck:expect(remote_mob, is_started, fun(_, _) -> true end),

    State = #state{},
    ServiceName = my_service,
    {Result, NewState} = mob:handle_is_remotely_started(ServiceName, State),

    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [ServiceName])),
    ?assertEqual(1, meck:num_calls(remote_mob, is_started, [FakeNode, ServiceName])),
    ?assert(Result),
    stop_mocks([remote_mob, mob_dht]).

should_known_if_a_service_is_locally_started_test() ->
    start_mocks([service_supervisor]),
    meck:expect(service_supervisor, is_started, fun(_ServiceName) -> true end),

    State = #state{},
    ServiceName = my_service,
    {Ret, NewState} = mob:handle_is_started(ServiceName, State),

    ?assertEqual(1, meck:num_calls(service_supervisor, is_started, [ServiceName])),
    ?assertEqual(State, NewState),
    ?assert(Ret),
    stop_mocks([service_supervisor]).

should_add_a_child_a_to_a_remotely_spawned_parent_test() ->
    start_mocks([mob_dht, remote_mob]),
    FakeNode = self(),

    meck:expect(mob_dht, where_deployed, fun(_ServiceName) -> {found, FakeNode} end),
    meck:expect(remote_mob, add_child, fun(_Node, _Parent, _Child) -> ok end),

    State = #state{},
    Parent = parent,
    Child = child,
    {Ret, NewState} = mob:handle_remotely_add_child(Parent, Child, State),

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
