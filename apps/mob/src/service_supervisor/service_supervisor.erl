-module(service_supervisor).

-behaviour(gen_server).

-export([start_link/0]).
-export([is_started/1]).
-export([add_child/2]).
-export([restart/1]).
-export([run/2]).

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

run(Service, Services) ->
    log:notice("[~p] Run Service: ~p", [?MODULE, Service#service.name]),
    Children = build_children(Service, Services),
    gen_server:cast(?SERVER, {run, Service, Children}).

restart(ServicesNames) when is_list(ServicesNames) ->
    lists:foreach(fun restart/1, ServicesNames);
restart(ServiceName) ->
    gen_server:cast(?SERVER, {restart, ServiceName}).

is_started(ServiceName) ->
    gen_server:call(?SERVER, {is_started, ServiceName}).

add_child(ParentName, ChildName) ->
    gen_server:call(?SERVER, {add_child, ParentName, ChildName}).

%% Callbacks

init([]) ->
        {ok, #state{spawned = []}}.

handle_call({add_child, ParentName, ChildName}, _From, State) ->
    {Reply, NewState} = handle_add_child(ParentName, ChildName, State),
    {reply, Reply, NewState};
handle_call({is_started, ServiceName}, _From, State) ->
        {Reply, NewState} = handle_is_started(ServiceName, State),
        {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({run, Service, Children}, State) ->
    NewState = handle_run(Service, Children, State),
    {noreply, NewState};
handle_cast({restart, ServiceName}, State) ->
    NewState = handle_restart(ServiceName, State),
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

handle_run(Service, Children, State) ->
    NewState = spawn_service(Service, Children, State),
    start_service(Service, NewState),
    notify_parents(Service, NewState),
    NewState.

handle_restart(ServiceName, State) ->
    Ret = case is_spawned(ServiceName, State) of
            true -> start_service(ServiceName, State);
            false -> mob_router:restart(ServiceName)
        end,
    log:notice("[~p] Restarting Service: ~p [~p]", [?MODULE, ServiceName, Ret]),
    State.

spawn_service(Service, Children, State = #state{spawned = Spawned}) ->
    ServiceName = Service#service.name,
    case is_spawned(ServiceName, State) of
        false -> service:spawn(Service, Children),
                 State#state{spawned = [ServiceName | Spawned]};
        true -> State
    end.

start_service(#service{name = ServiceName, requires = Dependencies}, State) ->
    start_service(ServiceName, Dependencies, State);

start_service(ServiceName, State) when is_atom(ServiceName) ->
    Dependencies = service:dependencies(ServiceName),
    start_service(ServiceName, Dependencies, State).

start_service(ServiceName, Dependencies, State) ->
    case are_started(Dependencies, State) of
        true -> service:restart(ServiceName);
        false -> log:notice("[~p] Dependencies for '~p' are not started", [?MODULE, ServiceName])
    end.

handle_add_child(ParentName, ChildName, State) ->
    Reply = notify_parent(ParentName, ChildName, State),
    {Reply, State}.
handle_is_started(ServiceName, State) ->
    Reply = is_locally_started(ServiceName, State),
    {Reply, State}.

is_spawned(ServiceName, #state{spawned = Spawned}) ->
    lists:member(ServiceName, Spawned).

are_started(Dependencies, State) ->
    lists:all(fun(Service) -> is_globally_started(Service, State) end, Dependencies).

is_globally_started(ServiceName, State) ->
    case is_spawned(ServiceName, State) of
        true -> service:is_started(ServiceName);
        false -> mob_router:is_started(ServiceName)
    end.

is_locally_started(ServiceName, State) ->
    case is_spawned(ServiceName, State) of
        true -> service:is_started(ServiceName);
        _ -> false
    end.

build_children(Service, Services) ->
    build_children(Service, Services, []).

build_children(_Service, [], Children) ->
    Children;
build_children(Service, [C | Services], Children) ->
    ChildName = C#service.name,
    case lists:member(Service#service.name, C#service.requires) of
        true -> build_children(Service, Services, [ChildName | Children]);
        false -> build_children(Service, Services, Children)
    end.

notify_parents(Service, State) ->
    Parents = Service#service.requires,
    Child = Service#service.name,
    lists:foreach(fun(P) -> notify_parent(P, Child, State) end, Parents).

notify_parent(Parent, Child, State) ->
    case is_spawned(Parent, State) of
        true -> service:add_child(Parent, Child);
        false -> mob_router:add_child(Parent, Child)
    end.

-ifdef(TEST).
-compile([export_all]).
-endif.
