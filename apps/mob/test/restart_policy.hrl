-include_lib("eunit/include/eunit.hrl").

always_restart_policy_test() ->
    ?assert(restart_policy:need_restart(always, 0)),
    ?assert(restart_policy:need_restart(always, -1)),
    ?assert(restart_policy:need_restart(always, 1)),
    ?assert(restart_policy:need_restart(always, 127)).

on_error_restart_policy_test() ->
    ?assert(restart_policy:need_restart(on_error, -1)),
    ?assert(restart_policy:need_restart(on_error, 1)),

    ?assert(not restart_policy:need_restart(on_error, 0)).

none_restart_policy_test() ->
    ?assert(not restart_policy:need_restart(none, -1)),
    ?assert(not restart_policy:need_restart(none, 1)),
    ?assert(not restart_policy:need_restart(none, 0)).
