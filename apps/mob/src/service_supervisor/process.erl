-module(process).

-export([exec/1]).
-export([stop/1]).
-export([status/1]).

exec(Command) ->
    {ok, Pid, OSPid} = exec:run(Command, [monitor]),
    {Pid, OSPid}.

stop(ProcessPid) ->
    %%XXX what if doesn't exit?
    exec:stop(ProcessPid).

status(normal) -> {exit_code, 0};
status({exit_status, Exit}) ->
    case exec:status(Exit) of
        {status, E} -> {exit_code, E};
        {signal, _Signal, _} -> {signal, Exit}
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/process.hrl").
-endif.
