-module(service).

-behaviour(gen_fsm).

-export([spawn/1]).
-export([start/1]).
-export([is_started/1]).
-export([terminate/1]).

-export([init/1,
     started/2,
     stopped/2,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include("service.hrl").

-record(state, {service, os_pid, exec_pid}).

spawn(Service = #service{name = ServiceName}) ->
    gen_fsm:start({local, ServiceName}, ?MODULE, [Service], []).

start(ServiceName) ->
    gen_fsm:send_event(ServiceName, start).

is_started(ServiceName) ->
    started =:= get_state(ServiceName).

terminate(ServiceName) ->
    gen_fsm:send_all_state_event(ServiceName, terminate).

get_state(ServiceName) ->
    gen_fsm:sync_send_all_state_event(ServiceName, get_state).

%% gen_fsm callbacks

init([Service]) ->
    process_flag(trap_exit, true),
    log:notice("[~p] Spawned Service: ~p", [?MODULE, Service#service.name]),
    {ok, stopped, #state{service = Service}}.

stopped(start, State = #state{service = Service}) ->
    {NextState, NewState} = handle_start(Service, State),
    log:notice("[~p] Started '~p' with PID ~p", [?MODULE, Service#service.name, NewState#state.os_pid]),
    {next_state, NextState, NewState};
stopped(_Event, State) ->
    {next_state, stopped, State}.

started(_Event, State) ->
    {next_state, started, State}.

handle_event(terminate, _StateName, S) ->
    {stop, normal, S};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    Reply = StateName,
    {reply, Reply, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({'DOWN', _, process, FromExecPid, ExitInfo}, _, State = #state{exec_pid = ExecPid})
      when FromExecPid =:= ExecPid ->
    {NextState, NewState} = handle_down(ExitInfo, State),
    {next_state, NextState, NewState};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Handlers

handle_start(#service{command = Command}, State) ->
    {Pid, OSPid} = process:exec("bash -c \"" ++ Command ++ "\""),
    {started, State#state{os_pid = OSPid, exec_pid = Pid}}.

handle_down(ExitInfo, #state{service = Service}) ->
    {exit_status, ExitStatus} = ExitInfo,
    CleanedState = #state{service = Service},

    log:notice("[~p] ~p exited with exit-status ~p", [?MODULE, Service#service.name, ExitStatus]),
    {stopped, CleanedState}.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/service.hrl").
-endif.
