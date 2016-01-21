-include_lib("eunit/include/eunit.hrl").

translate_normal_exit_to_zero_exit_code_test() ->
    ExitInfo = normal,
    ?assertEqual({exit_code, 0}, process:translate_status(ExitInfo)).

translate_successfully_exit_to_its_exit_code_test() ->
    ExitInfo = {exit_status, 256},
    ?assertEqual({exit_code, 1}, process:translate_status(ExitInfo)).

translate_signal_exit_to_signal_code_test() ->
    ExitInfo = {exit_status, 15},
    ?assertEqual({signal, 15}, process:translate_status(ExitInfo)).
