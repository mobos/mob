-include_lib("eunit/include/eunit.hrl").

should_find_the_correct_node_and_deploy_a_service_test() ->
    meck:new(service, [non_strict]),
    meck:new(discovery, [non_strict]),
    meck:new(remote_mob, [non_strict]),

    Service = "{
                 \"name\": \"my_service\",
                 \"command\": \"a command\"
               }",

    FakePeer = self(),
    FakeNode = fake_node,
    FakeParsedService = parsed_service,

    meck:expect(service, parse, fun(_) -> {ok, FakeParsedService} end),
    meck:expect(discovery, find_available_node, fun(_, _) -> {ok, FakeNode} end),
    meck:expect(remote_mob, run, fun(_, _) -> ok end),

    State = #state{peer = FakePeer},

    {Reply, NewState} = mob:handle_deploy(Service, State),

    ?assertEqual(1, meck:num_calls(service, parse, [Service])),
    ?assertEqual(1, meck:num_calls(discovery, find_available_node, [FakePeer, FakeParsedService])),
    ?assertEqual(1, meck:num_calls(remote_mob, run, [FakeNode, FakeParsedService])),
    ?assertEqual(State, NewState),
    ?assertEqual(FakeNode, Reply),


    meck:validate(service),
    meck:validate(discovery),
    meck:validate(remote_mob),
    meck:unload(service),
    meck:unload(discovery),
    meck:unload(remote_mob).

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

    meck:validate(service),
    meck:unload(service).
