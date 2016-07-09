-include_lib("eunit/include/eunit.hrl").
-include_lib("test_macro.hrl").

-define(FAKE_NODE, '0000@fakenode').
-define(SERVICE_NAME, 'my_service').

start() ->
    meck:new(remote_mob, [no_link]),
    meck:new(mob_dht, [no_link]).

teardown(_) ->
    ?assert(meck:validate(remote_mob)),
    ?assert(meck:validate(mob_dht)),
    meck:unload(remote_mob),
    meck:unload(mob_dht).

mob_router_suite_test_() ->
     [?setup(fun locate_a_service_and_ask_if_it_is_started/1),
      ?setup(fun locate_a_parent_service_and_add_a_child_to_it/1)].

locate_a_service_and_ask_if_it_is_started(_) ->
    meck:expect(mob_dht, where_deployed, fun(?SERVICE_NAME) -> {found, ?FAKE_NODE} end),
    meck:expect(remote_mob, is_started, fun(?FAKE_NODE, ?SERVICE_NAME) -> true end),

    Result = mob_router:is_started(?SERVICE_NAME),

    [?_assert(Result)].


locate_a_parent_service_and_add_a_child_to_it(_) ->
    meck:expect(mob_dht, where_deployed, fun(?SERVICE_NAME) -> {found, ?FAKE_NODE} end),
    meck:expect(remote_mob, add_child, fun(?FAKE_NODE, ?SERVICE_NAME, another_service) -> ok end),

    Result = mob_router:add_child(?SERVICE_NAME, another_service),

    [?_assertEqual(ok, Result)].
