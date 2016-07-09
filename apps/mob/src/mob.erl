-module(mob).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([join/1]).
-export([run/1]).
-export([deploy/1]).
-export([node_name/0]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-include("service_supervisor/service.hrl").

-record(state, {}).

start_link() ->
    gen_server:start_link({local, mob}, ?MODULE, [], []).

deploy(Service) ->
    gen_server:call(mob, {deploy, Service}).

run(Service) ->
    gen_server:cast(mob, {run, Service}).

join(NodeName) ->
    gen_server:call(mob, {join, NodeName}).

node_name() ->
    node().

%% Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({is_started, ServiceName}, _From, State) ->
    {Reply, NewState} = handle_is_started(ServiceName, State),
    {reply, Reply, NewState};

handle_call(peer, _From, State) ->
    Reply = mob_dht:peer(),
    {reply, Reply, State};

handle_call({join, NodeName}, _From, State) ->
    ConnectionResult = mob_dht:join(NodeName),
    {reply, ConnectionResult, State};

handle_call({deploy, Service}, _From, State) ->
    {Reply, NewState} = handle_deploy(Service, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({run, Service}, State) ->
    NewState = handle_run(Service, State),
    {noreply, NewState};
handle_cast({restart, Service}, State) ->
    NewState = handle_restart(Service, State),
    {noreply, NewState};
handle_cast({add_child, Parent, Child}, State) ->
    NewState = handle_add_child(Parent, Child, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Handlers

handle_deploy(Service, State) ->
    Reply = case service_parser:parse(Service) of
                {ok, ParsedService} -> mob_router:deploy(ParsedService);
                {error, Error} -> Error
            end,
    {Reply, State}.

handle_run(Service, State) ->
    %% XXX Currently the service is annunced *BEFORE* the service
    %% is really spawned. To be sure we should notify this module
    %% when the service is really spawned and proceed with the
    %% announce
    log:notice("[~p] Run request for ~p", [?MODULE, Service#service.name]),
    Services = mob_dht:services(),
    ServicesList = sets:to_list(Services),
    service_supervisor:run(Service, ServicesList),
    mob_dht:announce_spawned_service(Service, node_name()),
    State.

handle_is_started(ServiceName, State) ->
    Reply = service_supervisor:is_started(ServiceName),
    {Reply, State}.

handle_restart(ServiceName, State) ->
    service_supervisor:restart(ServiceName),
    State.

handle_add_child(Parent, Child, State) ->
    service_supervisor:add_child(Parent, Child),
    State.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/mob.hrl").
-endif.
