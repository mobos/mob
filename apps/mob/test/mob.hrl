-include_lib("eunit/include/eunit.hrl").

should_find_the_correct_node_and_deploy_a_service_test() ->
    start_mocks([service, discovery, remote_mob]),

    Service = "{
                 \"name\": \"my_service\",
                 \"command\": \"a command\"
               }",

    FakePeer = self(),
    FakeNode = fake_node,
    FakeParsedService = parsed_service,

    meck:expect(service, parse, fun(_) -> {ok, FakeParsedService} end),
    meck:expect(discovery, where_deployed, fun(_, _) -> {error, not_found} end),
    meck:expect(discovery, find_available_node, fun(_, _) -> {ok, FakeNode} end),
    meck:expect(remote_mob, run, fun(_, _) -> ok end),

    State = #state{peer = FakePeer},

    {Reply, NewState} = mob:handle_deploy(Service, State),

    ?assertEqual(1, meck:num_calls(service, parse, [Service])),
    ?assertEqual(1, meck:num_calls(discovery, where_deployed, [FakePeer, FakeParsedService])),
    ?assertEqual(1, meck:num_calls(discovery, find_available_node, [FakePeer, FakeParsedService])),
    ?assertEqual(1, meck:num_calls(remote_mob, run, [FakeNode, FakeParsedService])),
    ?assertEqual(State, NewState),
    ?assertEqual(FakeNode, Reply),

    stop_mocks([service, discovery, remote_mob]).


should_not_deploy_an_already_deployed_service_test() ->
    start_mocks([service, discovery]),

    Service = "{
                 \"name\": \"my_service\",
                 \"command\": \"a command\"
               }",

    FakePeer = self(),
    FakeNode = fake_node,
    FakeParsedService = parsed_service,

    meck:expect(service, parse, fun(_) -> {ok, FakeParsedService} end),
    meck:expect(discovery, where_deployed, fun(_, _) -> {found, FakeNode} end),

    State = #state{peer = FakePeer},
    {Reply, NewState} = mob:handle_deploy(Service, State),

    ?assertEqual(1, meck:num_calls(service, parse, [Service])),
    ?assertEqual(1, meck:num_calls(discovery, where_deployed, [FakePeer, FakeParsedService])),
    ?assertEqual(State, NewState),
    ?assertEqual('already_deployed', Reply),

    stop_mocks([service, discovery]).

should_reply_with_an_error_message_if_the_service_isnt_correct_test() ->
    meck:new(service, [non_strict]),
    WrongService = "[ a wrong service }",
    FakePeer = self(),

    State = #state{peer = FakePeer},
    ErrorMessage = format_error,

    meck:expect(service, parse, fun(_Service) -> {error, ErrorMessage} end),
    {Reply, NewState} = mob:handle_deploy(WrongService, State),

    ?assertEqual(ErrorMessage, Reply),
    ?assertEqual(State, NewState),

    stop_mocks([service]).

start_mocks([]) -> ok;
start_mocks([Mod | Modules]) ->
    meck:new(Mod, [non_strict]),
    start_mocks(Modules).

stop_mocks([]) -> ok;
stop_mocks([Mod | Modules]) ->
    ?assert(meck:validate(Mod)),
    meck:unload(Mod),
    stop_mocks(Modules).
