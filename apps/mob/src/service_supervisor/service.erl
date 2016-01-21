-module(service).

-behaviour(gen_fsm).

-export([spawn/2]).
-export([start/1]).
-export([dependencies/1]).
-export([stop/1]).
-export([restart/1]).
-export([is_started/1]).
-export([add_child/2]).
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

-record(state, {service, children, process}).

spawn(Service = #service{name = ServiceName}, Children) ->
    gen_fsm:start({local, ServiceName}, ?MODULE, [Service, Children], []).

start(ServiceName) ->
    gen_fsm:send_event(ServiceName, start).

stop(ServiceName) ->
    gen_fsm:send_event(ServiceName, stop).

is_started(ServiceName) ->
    started =:= get_state(ServiceName).

terminate(ServiceName) ->
    gen_fsm:send_all_state_event(ServiceName, terminate).

get_state(ServiceName) ->
    gen_fsm:sync_send_all_state_event(ServiceName, get_state).

dependencies(ServiceName) ->
    gen_fsm:sync_send_all_state_event(ServiceName, dependencies).

add_child(ServiceName, Child) ->
    gen_fsm:sync_send_all_state_event(ServiceName, {add_child, Child}).

restart(ServiceName) ->
    stop(ServiceName),
    start(ServiceName).

%% gen_fsm callbacks

init([Service, Children]) ->
    process_flag(trap_exit, true),
    log:notice("[~p] Spawned Service: ~p", [?MODULE, Service#service.name]),
    {ok, stopped, #state{service = Service, children = Children}}.

stopped(start, State = #state{service = Service}) ->
    {NextState, NewState} = handle_start(Service, State),
    OsPid = process:os_pid(NewState#state.process),
    log:notice("[~p] Started ~p with PID ~p", [?MODULE, Service#service.name, OsPid]),
    {next_state, NextState, NewState};
stopped(stop, State) ->
    {next_state, stopped, State};
stopped(_Event, State) ->
    {next_state, stopped, State}.

started(stop, State) ->
    {NextState, NewState} = handle_stop(State),
    {next_state, NextState, NewState};
started(_Event, State) ->
    {next_state, started, State}.

handle_event(restart, _StateName, S) ->
    {stop, normal, S};
handle_event(terminate, _StateName, S) ->
    {stop, normal, S};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({add_child, Child}, _From, StateName, State = #state{children = Children}) ->
    %% XXX children should be a set
    Service = State#state.service,
    log:notice("[~p] Adding ~p as child for ~p", [?MODULE, Child, Service#service.name]),
    NewChild = case lists:member(Child, Children) of
                   true -> Children;
                   false -> [Child | Children]
               end,
    {reply, ok, StateName, State#state{children = NewChild}};
handle_sync_event(get_state, _From, StateName, State) ->
    Reply = StateName,
    {reply, Reply, StateName, State};
handle_sync_event(dependencies, _From, StateName, State = #state{service = Service}) ->
    Reply = Service#service.requires,
    {reply, Reply, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({'EXIT', FromProcess, {shutdown, ExitInfo}}, _, State = #state{process = Process})
      when FromProcess =:= Process ->
    {NextState, NewState} = handle_down(ExitInfo, State),
    {next_state, NextState, NewState};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Handlers

handle_stop(State = #state{process = Process, service = Service}) ->
    Result = process:stop(Process),
    log:notice("[~p] Stopping ~p [~p]", [?MODULE, Service#service.name, Result]),
    CleanedState = State#state{process = undefined},
    {stopped, CleanedState}.

handle_start(#service{params = Params}, State = #state{children = Children}) ->
    #{"command" := Command} = Params,
    {ok, Process} = process:exec(["/bin/bash", "-c", Command]),
    service_supervisor:restart(Children),
    {started, State#state{process = Process}}.

handle_down(ExitCode, #state{service = Service, children = Children}) ->
    log:notice("[~p] ~p Exited with ~p", [?MODULE, Service#service.name, ExitCode]),
    case restart_policy:need_restart(Service#service.restart, ExitCode) of
        true -> service_supervisor:restart(Service#service.name);
        _ -> ok
    end,
    CleanedState = #state{service = Service, children = Children},
    {stopped, CleanedState}.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/service.hrl").
-endif.
