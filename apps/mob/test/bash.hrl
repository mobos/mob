-include_lib("eunit/include/eunit.hrl").

execute_process_with_the_specified_command_test() ->
    FakeFrom = self(),
    ProcessPid = self(),

    InitState = #state{params = #{"command" => "a command"}},

    meck:new(process),
    meck:expect(process, exec, fun(_) -> {ok, ProcessPid} end),

    {reply, ok, #state{process = Process}} = bash:handle_call(start, FakeFrom, InitState),

    ?assertEqual(Process, ProcessPid).
