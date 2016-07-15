-module(process).

-behaviour(gen_server).

-export([exec/1, exec/2]).
-export([syn_exec/1]).
-export([stop/1]).
-export([os_pid/1]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-record(state, {owner, os_pid, exec_pid}).

exec(Command) ->
    exec(Command, []).
exec(Command, Options) ->
    gen_server:start_link(?MODULE, [Command, self(), Options], []).

syn_exec(Command) ->
    case exec:run(Command, [sync, stdout]) of
        {_, []} -> {ok, translate_status(normal)};
        {_, [{stdout, Stdout}]} -> {ok, translate_status(normal), Stdout};
        {_, [ExitInfo, {stdout, Stdout}]} -> {ok, translate_status(ExitInfo), Stdout}
    end.

stop(ProcessPid) ->
    gen_server:call(ProcessPid, stop).

os_pid(ProcessPid) ->
    gen_server:call(ProcessPid, os_pid).

%% Callbacks

init([Command, Owner, Options]) ->
    process_flag(trap_exit, true),
    {ok, ExecPid, OSPid} = exec:run_link(Command, Options),
    {ok, #state{os_pid = OSPid, exec_pid = ExecPid, owner = Owner}}.

handle_call(stop, _From, S = #state{exec_pid = ExecPid}) ->
    exec:stop(ExecPid),
    {reply, ok, S};
handle_call(os_pid, _From, S = #state{os_pid = OSPid}) ->
    {reply, OSPid, S};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({stdout, FromOsPid, FlushedStdout}, S = #state{os_pid = OsPid, owner = Owner}) when FromOsPid =:= OsPid ->
    Owner ! {stdout, self(), FlushedStdout},
    {noreply, S};
handle_info({'EXIT',FromExecPid,ExitInfo}, S = #state{exec_pid = ExecPid}) when FromExecPid =:= ExecPid ->
    {stop, {shutdown, translate_status(ExitInfo)}, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


translate_status(normal) -> {exit_code, 0};
translate_status({exit_status, Exit}) ->
    case exec:status(Exit) of
    {status, E} -> {exit_code, E};
    {signal, _Signal, _} -> {signal, Exit}
    end.

-ifdef(TEST).
-compile([export_all]).
-endif.
