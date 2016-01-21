-module(restart_policy).

-export([need_restart/2]).

need_restart(always, _ExitCode) -> true;
need_restart(on_error, ExitCode) when ExitCode =/= 0 -> true;
need_restart(on_error, _ExitCode) -> false;
need_restart(none, _ExitCode) -> false.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/restart_policy.hrl").
-endif.
