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

    Reply = mob:handle_deploy(Service),

    ?assertEqual(1, meck:num_calls(service_parser, parse, [Service])),
    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [ParsedService#service.name])),
    ?assertEqual(1, meck:num_calls(mob_dht, find_available_node, [ParsedService])),
    ?assertEqual(1, meck:num_calls(remote_mob, run, [FakeNode, ParsedService])),
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

    Reply = mob:handle_deploy(Service),

    ?assertEqual(1, meck:num_calls(service_parser, parse, [Service])),
    ?assertEqual(1, meck:num_calls(mob_dht, where_deployed, [ParsedService#service.name])),
    ?assertEqual('already_deployed', Reply),

    stop_mocks([service_parser, mob_dht]).

should_reply_with_an_error_message_if_the_service_isnt_correct_test() ->
    meck:new(service, [non_strict]),
    WrongService = "[ a wrong service }",
    FakePeer = self(),

    ErrorMessage = format_error,

    meck:expect(service, parse, fun(_Service) -> {error, ErrorMessage} end),
    Reply = mob:handle_deploy(WrongService),

    ?assertEqual(ErrorMessage, Reply),

    stop_mocks([service]).

should_known_if_a_service_is_locally_started_test() ->
    start_mocks([service_supervisor]),
    meck:expect(service_supervisor, is_started, fun(_ServiceName) -> true end),

    ServiceName = my_service,
    Ret = mob:handle_is_started(ServiceName),

    ?assertEqual(1, meck:num_calls(service_supervisor, is_started, [ServiceName])),
    ?assert(Ret),
    stop_mocks([service_supervisor]).

start_mocks([]) -> ok;
start_mocks([Mod | Modules]) ->
    meck:new(Mod, [non_strict]),
    start_mocks(Modules).

stop_mocks([]) -> ok;
stop_mocks([Mod | Modules]) ->
    ?assert(meck:validate(Mod)),
    meck:unload(Mod),
    stop_mocks(Modules).
