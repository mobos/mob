-module(process).

-export([exec/1]).
-export([stop/1]).

exec(Command) ->
    {ok, Pid, OSPid} = exec:run(Command, [monitor]),
    {Pid, OSPid}.

stop(ProcessPid) ->
    %%XXX what if doesn't exit?
    exec:stop(ProcessPid).
