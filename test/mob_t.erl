-module(mob_t).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/test_macro.hrl").

-include("apps/mob/src/service_supervisor/service.hrl").

-define(SERVICE_DESCRIPTOR, "{\"name\": \"my_service\",
                              \"provider\": \"bash\",
                              \"params\": {
                                    \"command\": \"a command\"
                              }}").
-define(PARSED_SERVICE, #service{name = 'my_service',
                                 provider = 'bash',
                                 params = #{
                                   "command" => "a command"}
                                }).
-define(FAKE_NODE, '0000@fakenode').

-record(state, {}).

start() ->
    meck:new(service_parser, [no_link]),
    meck:new(mob_router, [no_link]).

teardown(_) ->
    ?assert(meck:validate(service_parser)),
    ?assert(meck:validate(mob_router)),
    meck:unload(service_parser),
    meck:unload(mob_router).

mob_suite_test_() ->
     [?setup(fun parse_a_service_descriptor_and_deploy/1),
      ?setup(fun reply_with_the_selected_node_when_a_service_is_deployed/1),
      ?setup(fun reply_with_an_error_message_if_the_service_is_already_deployed/1),
      ?setup(fun reply_with_an_error_message_if_the_service_descriptor_has_a_wrong_format/1)].

parse_a_service_descriptor_and_deploy(_) ->
    meck:expect(service_parser, parse, fun(_) -> {ok, ?PARSED_SERVICE} end),
    meck:expect(mob_router, deploy, fun(_) -> ?FAKE_NODE end),

    mob:handle_deploy(?SERVICE_DESCRIPTOR),

    [?_assertEqual(1, meck:num_calls(service_parser, parse, [?SERVICE_DESCRIPTOR])),
     ?_assertEqual(1, meck:num_calls(mob_router, deploy, [?PARSED_SERVICE]))].


reply_with_the_selected_node_when_a_service_is_deployed(_) ->
    meck:expect(service_parser, parse, fun(_) -> {ok, ?PARSED_SERVICE} end),
    meck:expect(mob_router, deploy, fun(_) -> ?FAKE_NODE end),

    Reply = mob:handle_deploy(?SERVICE_DESCRIPTOR),

    [?_assertEqual(?FAKE_NODE, Reply)].

reply_with_an_error_message_if_the_service_is_already_deployed(_) ->
    meck:expect(service_parser, parse, fun(_) -> {ok, ?PARSED_SERVICE} end),
    meck:expect(mob_router, deploy, fun(_) -> already_deployed end),

    Reply = mob:handle_deploy(?SERVICE_DESCRIPTOR),

    [?_assertEqual('already_deployed', Reply)].


reply_with_an_error_message_if_the_service_descriptor_has_a_wrong_format(_) ->
    WrongService = "[ a wrong service }",
    ErrorMessage = format_error,
    meck:expect(service_parser, parse, fun(_Service) -> {error, ErrorMessage} end),

    Reply = mob:handle_deploy(WrongService),

    [?_assertEqual(ErrorMessage, Reply)].
