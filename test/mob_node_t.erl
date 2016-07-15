-module(mob_node_t).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/test_macro.hrl").

start() ->
    meck:new(service_supervisor, [no_link]).

teardown(_) ->
    ?assert(meck:validate(service_supervisor)),
    meck:unload(service_supervisor).

mob_node_suite_test_() ->
     [?setup(fun check_if_a_service_is_started/1)].

check_if_a_service_is_started(_) ->
    meck:expect(service_supervisor, is_started, fun(_ServiceName) -> true end),
    ServiceName = my_service,

    Ret = mob_node:handle_is_started(ServiceName),

    [?_assertEqual(1, meck:num_calls(service_supervisor, is_started, [ServiceName])),
     ?_assert(Ret)].
