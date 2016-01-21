-module(restart_policy).

-export([need_restart/2]).

need_restart(always, {exit_code, _}) -> true;
need_restart(always, {signal, _}) -> true;

need_restart(on_error, {exit_code, 0}) -> false;
need_restart(on_error, {exit_code, _}) -> true;
need_restart(on_error, {signal, _}) -> false;

need_restart(none, _ExitInfo) -> false.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/restart_policy.hrl").
-endif.
