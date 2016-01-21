-include_lib("eunit/include/eunit.hrl").

always_restart_policy_test() ->
    ?assert(restart_policy:need_restart(always, {exit_code, 0})),
    ?assert(restart_policy:need_restart(always, {exit_code, -1})),
    ?assert(restart_policy:need_restart(always, {signal, 1})),
    ?assert(restart_policy:need_restart(always, {exit_code, 127})).

on_error_restart_policy_test() ->
    ?assert(restart_policy:need_restart(on_error, {exit_code, -1})),
    ?assert(restart_policy:need_restart(on_error, {exit_code, 1})),

    ?assert(not restart_policy:need_restart(on_error, {signal, 15})),
    ?assert(not restart_policy:need_restart(on_error, {exit_code, 0})).

none_restart_policy_test() ->
    ?assert(not restart_policy:need_restart(none, {exit_code, -1})),
    ?assert(not restart_policy:need_restart(none, {signal, 1})),
    ?assert(not restart_policy:need_restart(none, {exit_code, 0})).
