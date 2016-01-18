-module(service_supervisor).

-behaviour(gen_server).

-export([start_link/0]).
-export([is_started/1]).
-export([run/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("service.hrl").

-define(SERVER, ?MODULE).

-record(state, {spawned}).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run(Service) ->
    log:log("[~p] Run Service: ~p", [?MODULE, Service#service.name]),
    gen_server:cast(?SERVER, {run, Service}).

is_started(ServiceName) ->
    gen_server:call(?SERVER, {is_started, ServiceName}).

%% Callbacks

init([]) ->
        {ok, #state{spawned = []}}.

handle_call({is_started, ServiceName}, _From, State) ->
        {Reply, NewState} = handle_is_started(ServiceName, State),
        {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({run, Service}, State) ->
    NewState = handle_run(Service, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%% Handlers

handle_run(Service, State = #state{spawned = Spawned}) ->
    ServiceName = Service#service.name,
    Dependencies = Service#service.requires,

    NewState = case is_spawned(ServiceName, State) of
        false -> service:spawn(Service),
                 State#state{spawned = [ServiceName | Spawned]};
        true -> State
    end,
    case are_started(Dependencies, State) of
        true -> service:start(ServiceName);
        false -> dependencies_not_started
    end,
    NewState.

handle_is_started(ServiceName, State) ->
    Reply = is_locally_started(ServiceName, State),
    {Reply, State}.

is_spawned(ServiceName, #state{spawned = Spawned}) ->
    lists:member(ServiceName, Spawned).

are_started(Dependencies, State) ->
    lists:all(fun(Service) -> is_globally_started(Service, State) end, Dependencies).

is_globally_started(ServiceName, State) ->
    case is_locally_started(ServiceName, State) of
        true -> true;
        false -> mob:is_remotely_started(ServiceName)
    end.

is_locally_started(ServiceName, State) ->
    case is_spawned(ServiceName, State) of
        true -> service:is_started(ServiceName);
        _ -> false
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/service_supervisor.hrl").
-endif.
