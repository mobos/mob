-include_lib("eunit/include/eunit.hrl").

should_run_a_service_test() ->
    meck:new(service, [non_strict]),
    FakeServicePid = self(),

    meck:expect(service, spawn, fun(_Service) -> {ok, FakeServicePid} end),
    meck:expect(service, start, fun(_ServiceName) -> ok end),

    State = #state{spawned = []},
    Service = #service{name = "myservice", command = "my command"},
    NewState = handle_run(Service, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [Service])),
    ?assertEqual(1, meck:num_calls(service, start, [Service#service.name])),
    ?assertEqual(#state{spawned = [Service#service.name]}, NewState),

    ?assert(meck:validate(service)),
    meck:unload(service).

should_not_try_to_spawn_an_already_spawned_service_test() ->
    meck:new(service, [non_strict]),

    meck:expect(service, start, fun(_ServiceName) -> ok end),

    State = #state{spawned = ["myservice"]},
    Service = #service{name = "myservice", command = "my command"},
    NewState = handle_run(Service, State),

    ?assertEqual(1, meck:num_calls(service, start, [Service#service.name])),
    ?assertEqual(State, NewState),
    ?assert(meck:validate(service)),
    meck:unload(service).
