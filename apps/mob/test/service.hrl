-include_lib("eunit/include/eunit.hrl").

-define(SIGTERM, 15).

should_parse_a_service_test() ->
    Name = my_service,
    Command = "a command",

    Service = "{
                 \"name\": \"my_service\",
                 \"command\": \"a command\"
               }",

    {ok, ParsedService} = service:parse(Service),

    ?assertEqual(Name, ParsedService#service.name),
    ?assertEqual(Command, ParsedService#service.command).

should_fail_if_the_format_isnt_json_test() ->
    Service = "{ not_valid_json",

    {error, Error} = service:parse(Service),

    ?assertEqual(Error, format_error).

should_spawn_a_service_registering_its_name_test() ->
    Service = #service{name = my_service, command = "a command"},

    {ok, ServicePid} = service:spawn(Service),
    service:terminate(ServicePid),

    ?assert(lists:member(Service#service.name, erlang:registered())).

start_a_service_exec_its_command_in_a_bash_shell_test() ->
    meck:new(process, [non_strict]),
    FakeOsPid = FakeExecPid = self(),
    meck:expect(process, exec, fun(_Command) -> {FakeOsPid, FakeExecPid} end),

    Service = #service{name = my_service, command = "my_command"},
    {started, NewState} = service:handle_start(Service, #state{service = Service}),

    ?assertEqual(1, meck:num_calls(process, exec, ["bash -c \"" ++ Service#service.command ++ "\""])),
    ?assertEqual(FakeOsPid, NewState#state.ospid),
    ?assertEqual(FakeExecPid, NewState#state.exec_pid),

    ?assert(meck:validate(process)),
    meck:unload(process).

when_a_started_service_goes_down_become_stopped_test() ->
    Service = #service{name = my_service, command = "my_command"},

    FakeOsPid = FakeExecPid = self(),
    ExitInfo = {exit_status, ?SIGTERM},
    State = #state{service = Service, ospid = FakeOsPid, exec_pid = FakeExecPid},
    {NextState, CleanedState} = service:handle_down(ExitInfo, State),

    ?assertEqual(stopped, NextState),
    ?assertEqual(#state{service = Service}, CleanedState).
