-module(service_supervisor_t).

-include_lib("eunit/include/eunit.hrl").

-include("apps/mob/src/service_supervisor/service.hrl").

-define(SERVICE, #service{name = myservice,
                          provider = bash,
                          params = #{"command" => "my command"},
                          requires = []}).

-record(state, {spawned}).

should_run_a_service_test() ->
    meck:new(service, [non_strict]),
    FakeServicePid = self(),

    meck:expect(service, spawn, fun(_Service, _Children) -> {ok, FakeServicePid} end),
    meck:expect(service, restart, fun(_ServiceName) -> ok end),

    State = #state{spawned = []},
    Children = [],
    NewState = service_supervisor:handle_run(?SERVICE, Children, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [?SERVICE, Children])),
    ?assertEqual(1, meck:num_calls(service, restart, [?SERVICE#service.name])),
    ?assertEqual(#state{spawned = [?SERVICE#service.name]}, NewState),

    ?assert(meck:validate(service)),
    meck:unload(service).

should_not_try_to_spawn_an_already_spawned_service_test() ->
    meck:new(service, [non_strict]),

    meck:expect(service, restart, fun(_ServiceName) -> ok end),

    State = #state{spawned = [myservice]},

    Children = [],
    NewState = service_supervisor:handle_run(?SERVICE, Children, State),

    ?assertEqual(1, meck:num_calls(service, restart, [?SERVICE#service.name])),
    ?assertEqual(State, NewState),
    ?assert(meck:validate(service)),
    meck:unload(service).

should_spawn_but_not_start_a_service_if_its_dependencies_arent_started_test() ->
    meck:new(service, [non_strict]),
    meck:new(mob_router, [non_strict]),
    FakeServicePid = self(),
    DependencyName = first_dependency,
    State = #state{spawned = [DependencyName]},

    meck:expect(service, spawn, fun(_Service, _Children) -> {ok, FakeServicePid} end),
    meck:expect(service, add_child, fun(_Parent, _Child) -> ok end),
    meck:expect(service, restart, fun(_ServiceName) -> ok end),
    meck:expect(service, is_started, fun(_ServiceName) -> false end),
    meck:expect(mob_router, is_started, fun(_ServiceName) -> false end),

    Service = #service{name = myservice, provider = bash, requires = [DependencyName]},
    ExpectedState = #state{spawned = [Service#service.name, DependencyName]},
    Children = [],
    NewState = service_supervisor:handle_run(Service, Children, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [Service, Children])),
    ?assertEqual(1, meck:num_calls(service, add_child, [DependencyName, Service#service.name])),
    ?assertEqual(1, meck:num_calls(service, is_started, [DependencyName])),
    ?assertEqual(0, meck:num_calls(service, restart, [Service#service.name])),
    ?assertEqual(ExpectedState, NewState),
    ?assert(meck:validate(service)),
    ?assert(meck:validate(mob_router)),
    meck:unload(service),
    meck:unload(mob_router).

should_search_on_the_network_if_a_dependencies_isnt_found_locally_test() ->
    meck:new(service, [non_strict]),
    meck:new(mob_router, [non_strict]),

    FakeServicePid = self(),
    DependencyName = first_dependency,
    State = #state{spawned = []},

    meck:expect(service, spawn, fun(_Service, _Children) -> {ok, FakeServicePid} end),
    meck:expect(mob_router, add_child, fun(_Parent, _Child) -> ok end),
    meck:expect(service, restart, fun(_ServiceName) -> ok end),
    meck:expect(mob_router, is_started, fun(_ServiceName) -> true end),

    Service = #service{name = myservice, provider = bash, requires = [DependencyName]},
    ExpectedState = #state{spawned = [Service#service.name]},
    Children = [],
    NewState = service_supervisor:handle_run(Service, Children, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [Service, Children])),
    ?assertEqual(1, meck:num_calls(mob_router, add_child, [DependencyName, Service#service.name])),
    ?assertEqual(1, meck:num_calls(mob_router, is_started, [DependencyName])),
    ?assertEqual(1, meck:num_calls(service, restart, [Service#service.name])),
    ?assertEqual(ExpectedState, NewState),
    ?assert(meck:validate(service)),
    ?assert(meck:validate(mob_router)),
    meck:unload(service),
    meck:unload(mob_router).

should_build_a_list_of_children_test() ->
    Service = #service{name = 'parent', requires = []},

    ChildA = #service{name = 'childa', requires = ['parent']},
    ChildB = #service{name = 'childb', requires = ['parent']},
    Orphan = #service{name = 'orphan', requires = []},

    Children = service_supervisor:build_children(Service, [ChildA, Orphan, ChildB]),

    ?assertEqual(['childb', 'childa'], Children).

should_restart_a_locally_spawned_service_test() ->
    meck:new(service, [non_strict]),
    meck:expect(service, dependencies, fun(_ServiceName) -> [] end),
    meck:expect(service, restart, fun(_ServiceName) -> ok end),

    ServiceName = 'myservice',
    State = #state{spawned = [ServiceName]},
    service_supervisor:handle_restart(ServiceName, State),

    ?assertEqual(1, meck:num_calls(service, restart, [ServiceName])),
    ?assertEqual(1, meck:num_calls(service, dependencies, [ServiceName])),
    ?assert(meck:validate(service)),
    meck:unload(service).

should_delegate_restart_request_for_a_non_locally_spawned_service_test() ->
    meck:new(mob_router, [non_strict]),
    meck:expect(mob_router, restart, fun(_ServiceName) -> ok end),

    ServiceName = 'myservice',
    State = #state{spawned = []},
    service_supervisor:handle_restart(ServiceName, State),

    ?assertEqual(1, meck:num_calls(mob_router, restart, [ServiceName])),
    ?assert(meck:validate(mob_router)),
    meck:unload(mob_router).

should_add_a_service_as_a_child_for_its_parents_test() ->
    meck:new(mob_router, [non_strict]),
    meck:new(service, [non_strict]),

    meck:expect(service, add_child, fun(_Parent, _Child) -> ok end),
    meck:expect(mob_router, add_child, fun(_Parent, _Child) -> ok end),

    ParentA = parentA,
    ParentB = parentB,
    Service = #service{name = my_service, requires = [ParentA, ParentB]},

    State = #state{spawned = [ParentA]},
    service_supervisor:notify_parents(Service, State),

    ?assertEqual(1, meck:num_calls(service, add_child, [ParentA, Service#service.name])),
    ?assertEqual(1, meck:num_calls(mob_router, add_child, [ParentB, Service#service.name])),
    ?assert(meck:validate(mob_router)),
    ?assert(meck:validate(service)),
    meck:unload(mob_router),
    meck:unload(service).
