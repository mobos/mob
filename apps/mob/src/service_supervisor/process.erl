-module(process).

-behaviour(gen_server).

-export([exec/1]).
-export([stop/1]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-record(state, {os_pid, exec_pid}).

exec(Command) ->
    gen_server:start_link(?MODULE, [Command], []).

stop(ProcessPid) ->
    gen_server:call(ProcessPid, stop).

%% Callbacks

init([Command]) ->
    process_flag(trap_exit, true),
    {ok, ExecPid, OSPid} = exec:run_link(Command, []),
    {ok, #state{os_pid = OSPid, exec_pid = ExecPid}}.

handle_call(stop, _From, S = #state{exec_pid = ExecPid}) ->
    exec:stop(ExecPid),
    {reply, ok, S};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
-include_lib("../../test/process.hrl").
-endif.
