-module(service).

-behaviour(gen_fsm).

-export([spawn/1]).
-export([start/1]).
-export([terminate/1]).
-export([parse/1]).

-export([init/1,
     started/2,
     spawned/2,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include("service.hrl").

-record(state, {service, ospid, exec_pid}).

parse(Service) ->
    BinaryService = list_to_binary(Service),
    case jsx:is_json(BinaryService) of
    false -> {error, format_error};
    true ->  {ok, json_to_service(BinaryService)}
    end.

json_to_service(BinaryService) ->
    ParsedService = jsx:decode(BinaryService),
    {<<"name">>, BinaryName} = lists:keyfind(<<"name">>, 1, ParsedService),
    {<<"command">>, BinaryCommand} = lists:keyfind(<<"command">>, 1, ParsedService),

    #service{
       name = binary_to_atom(BinaryName, utf8),
       command = binary_to_list(BinaryCommand)
    }.

spawn(Service = #service{name = ServiceName}) ->
    gen_fsm:start({local, ServiceName}, ?MODULE, [Service], []).

start(ServiceName) ->
    gen_fsm:send_event(ServiceName, start).

terminate(ServiceName) ->
    gen_fsm:send_all_state_event(ServiceName, terminate).

%% gen_fsm callbacks

init([Service]) ->
    process_flag(trap_exit, true),
    log:log("[~p] Spawned Service: ~p", [?MODULE, Service#service.name]),
    {ok, spawned, #state{service = Service}}.

spawned(start, State = #state{service = Service}) ->
    {NextState, NewState} = handle_start(Service, State),
    log:log("[~p] Started '~p' with PID ~p", [?MODULE, Service#service.name, NewState#state.ospid]),
    {next_state, NextState, NewState};
spawned(_Event, State) ->
    {next_state, spawned, State}.

started(_Event, State) ->
    {next_state, started, State}.

handle_event(terminate, _StateName, S) ->
    {stop, normal, S};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

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
    {started, State#state{ospid = OSPid, exec_pid = Pid}}.

handle_down(ExitInfo, #state{service = Service}) ->
    {exit_status, ExitStatus} = ExitInfo,
    CleanedState = #state{service = Service},

    log:log("[~p] ~p exited with exit-status ~p", [?MODULE, Service#service.name, ExitStatus]),
    {spawned, CleanedState}.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/service.hrl").
-endif.
