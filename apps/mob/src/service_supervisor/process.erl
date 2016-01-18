-module(process).

-export([exec/1]).

exec(Command) ->
    {ok, Pid, OSPid} = exec:run(Command, [monitor]),
    {Pid, OSPid}.
