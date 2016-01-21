-include_lib("eunit/include/eunit.hrl").

-define(FAKE_OSPID, self()).
-define(FAKE_EXECPID, self()).
-define(SIGTERM, 15).

-define(SERVICE_NAME, my_service).
-define(SERVICE_COMMAND, "my command").
-define(SERVICE_REQUIRES, [first_dependency, second_dependency]).
-define(SERVICE_CHILDREN, [childa, childb]).
-define(SIMPLE_SERVICE, #service{name = ?SERVICE_NAME, command = ?SERVICE_COMMAND}).

should_spawn_a_service_registering_its_name_test() ->
    Children = [],
    {ok, ServicePid} = service:spawn(?SIMPLE_SERVICE, Children),
    service:terminate(ServicePid),

    ?assert(lists:member(?SERVICE_NAME, erlang:registered())).

start_a_service_exec_its_command_in_a_bash_shell_test() ->
    meck:new(process, [non_strict]),
    meck:new(service_supervisor, [non_strict]),

    meck:expect(service_supervisor, restart, fun(_Children) -> ok end),
    meck:expect(process, exec, fun(_Command) -> {?FAKE_OSPID, ?FAKE_EXECPID} end),

    State = #state{service = ?SIMPLE_SERVICE, children = ?SERVICE_CHILDREN},
    {NextState, NewState} = service:handle_start(?SIMPLE_SERVICE, State),

    ?assertEqual(1, meck:num_calls(process, exec, [["/bin/bash", "-c", ?SERVICE_COMMAND]])),
    ?assertEqual(1, meck:num_calls(service_supervisor, restart, [?SERVICE_CHILDREN])),
    ?assertEqual(started, NextState),
    ?assertEqual(?FAKE_OSPID, NewState#state.os_pid),
    ?assertEqual(?FAKE_EXECPID, NewState#state.exec_pid),
    ?assert(meck:validate(process)),
    ?assert(meck:validate(service_supervisor)),

    meck:unload(process),
    meck:unload(service_supervisor).

when_a_started_service_goes_down_become_stopped_test() ->
    meck:new(restart_policy, [non_strict]),
    meck:expect(restart_policy, need_restart, fun(_Policy, _ExitCode) -> false end),

    ExitCode = {signal, ?SIGTERM},
    State = #state{service = ?SIMPLE_SERVICE, os_pid = ?FAKE_OSPID, exec_pid = ?FAKE_EXECPID},
    {NextState, CleanedState} = service:handle_down(ExitCode, State),

    ?assertEqual(stopped, NextState),
    ?assertEqual(#state{service = ?SIMPLE_SERVICE}, CleanedState),
    ?assert(meck:validate(restart_policy)),
    meck:unload(restart_policy).

when_a_started_service_goes_down_apply_its_restart_policy_test() ->
    meck:new(restart_policy, [non_strict]),
    meck:new(service_supervisor, [non_strict]),
    meck:expect(restart_policy, need_restart, fun(_Policy, _ExitCode) -> true end),
    meck:expect(service_supervisor, restart, fun(_ServiceName) -> ok end),

    FakeOsPid = 1111,
    RestartService = #service{name = always_restart, restart = always},
    ExitCode = {status, 0},

    State = #state{service = RestartService, os_pid = FakeOsPid},
    {NextState, CleanedState} = service:handle_down(ExitCode, State),

    ?assertEqual(1, meck:num_calls(restart_policy, need_restart, [RestartService#service.restart, ExitCode])),
    ?assertEqual(1, meck:num_calls(service_supervisor, restart, [RestartService#service.name])),
    ?assert(meck:validate(restart_policy)),
    ?assert(meck:validate(service_supervisor)),
    meck:unload(restart_policy),
    meck:unload(service_supervisor).
