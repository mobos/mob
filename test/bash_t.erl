-module(bash_t).

-include_lib("eunit/include/eunit.hrl").

-include("apps/mob/src/bash.hrl").

execute_process_with_the_specified_command_test() ->
    FakeFrom = self(),
    ExpectedProcessPid = self(),

    InitState = #state{params = #{"command" => "a command"}},

    meck:new(process),
    meck:expect(process, exec, fun(_) -> {ok, ExpectedProcessPid} end),

    {reply, ok, #state{process = ProcessPid}} = bash:handle_call(start, FakeFrom, InitState),

    ?assertEqual(ProcessPid, ExpectedProcessPid).
