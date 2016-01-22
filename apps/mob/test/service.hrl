-include_lib("eunit/include/eunit.hrl").

-define(FAKE_PROCESS, self()).
-define(SIGTERM, 15).

-define(SERVICE_NAME, my_service).
-define(SERVICE_CHILDREN, [childa, childb]).
-define(SERVICE_PROVIDER, bash).
-define(SERVICE_PARAMS, #{"command" => "my command"}).
-define(SIMPLE_SERVICE, #service{name = ?SERVICE_NAME,
                                 provider = ?SERVICE_PROVIDER,
                                 params = ?SERVICE_PARAMS,
                                 restart = none}).

should_spawn_a_service_registering_its_name_test() ->
    Children = [],
    {ok, ServicePid} = service:spawn(?SIMPLE_SERVICE, Children),
    service:terminate(ServicePid),

    ?assert(lists:member(?SERVICE_NAME, erlang:registered())).

start_a_service_exec_its_provider_test() ->
    meck:new(provider, [non_strict]),
    meck:new(service_supervisor, [non_strict]),

    FakeProvider = self(),
    meck:expect(service_supervisor, restart, fun(_Children) -> ok end),
    meck:expect(provider, init, fun(_ProviderName, _Params) -> {ok, FakeProvider} end),
    meck:expect(provider, start, fun(_Provider) -> ok end),

    State = #state{service = ?SIMPLE_SERVICE, provider = FakeProvider, children = ?SERVICE_CHILDREN},
    {NextState, NewState} = service:handle_start(?SIMPLE_SERVICE, State),

    ?assertEqual(1, meck:num_calls(provider, init, [?SERVICE_PROVIDER, ?SERVICE_PARAMS])),
    ?assertEqual(1, meck:num_calls(provider, start, [FakeProvider])),
    ?assertEqual(1, meck:num_calls(service_supervisor, restart, [?SERVICE_CHILDREN])),
    ?assertEqual(started, NextState),
    ?assert(meck:validate(provider)),
    ?assert(meck:validate(service_supervisor)),

    meck:unload(provider),
    meck:unload(service_supervisor).

when_a_started_service_goes_down_become_stopped_test() ->
    meck:new(restart_policy, [non_strict]),
    meck:expect(restart_policy, need_restart, fun(_Policy, _ExitCode) -> false end),

    ExitCode = {signal, ?SIGTERM},
    State = #state{service = ?SIMPLE_SERVICE},
    {NextState, CleanedState} = service:handle_down(ExitCode, State),

    ?assertEqual(stopped, NextState),
    ?assert(meck:validate(restart_policy)),
    meck:unload(restart_policy).

when_a_started_service_goes_down_apply_its_restart_policy_test() ->
    meck:new(restart_policy, [non_strict]),
    meck:new(service_supervisor, [non_strict]),
    meck:expect(restart_policy, need_restart, fun(_Policy, _ExitCode) -> true end),
    meck:expect(service_supervisor, restart, fun(_ServiceName) -> ok end),

    RestartService = #service{name = always_restart, restart = always},
    ExitCode = {status, 0},

    State = #state{service = RestartService},
    {NextState, CleanedState} = service:handle_down(ExitCode, State),

    ?assertEqual(1, meck:num_calls(restart_policy, need_restart, [RestartService#service.restart, ExitCode])),
    ?assertEqual(1, meck:num_calls(service_supervisor, restart, [RestartService#service.name])),
    ?assert(meck:validate(restart_policy)),
    ?assert(meck:validate(service_supervisor)),
    meck:unload(restart_policy),
    meck:unload(service_supervisor).
