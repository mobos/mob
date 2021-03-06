-module(mob_node).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([connect/1]).
-export([run/2]).
-export([is_started/2]).
-export([restart/2]).
-export([add_child/3]).
-export([name/0]).
-export([peer/1]).

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
    gen_server:start_link({local, mob_node}, mob_node, [], []).

connect(Node) ->
    net_kernel:connect_node(Node).

peer(Node) ->
    gen_server:call({mob_node, Node}, {peer}).

name() ->
    node().

run(Node, Service) ->
    gen_server:cast({mob_node, Node}, {run, Service}).

is_started(Node, Service) ->
    gen_server:call({mob_node, Node}, {is_started, Service}).

restart(Node, Service) ->
    gen_server:cast({mob_node, Node}, {restart, Service}).

add_child(Node, Parent, Child) ->
    gen_server:cast({mob_node, Node}, {add_child, Parent, Child}).

init([]) ->
    {ok, #state{}}.

handle_call({peer}, _From, State) ->
    Reply = handle_peer(),
    {reply, Reply, State};
handle_call({is_started, ServiceName}, _From, State) ->
    Reply = handle_is_started(ServiceName),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_child, Parent, Child}, State) ->
    handle_add_child(Parent, Child),
    {noreply, State};
handle_cast({restart, Service}, State) ->
    handle_restart(Service),
    {noreply, State};
handle_cast({run, Service}, State) ->
    handle_run(Service),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_run(Service) ->
    %% XXX Currently the service is annunced *BEFORE* the service
    %% is really spawned. To be sure we should notify this module
    %% when the service is really spawned and proceed with the
    %% announce
    log:notice("[~p] Run request for ~p", [?MODULE, Service#service.name]),
    Services = mob_dht:services(),
    ServicesList = sets:to_list(Services),
    service_supervisor:run(Service, ServicesList),
    mob_dht:announce_spawned_service(Service, name()).

handle_is_started(ServiceName) ->
    service_supervisor:is_started(ServiceName).

handle_restart(ServiceName) ->
    service_supervisor:restart(ServiceName).

handle_add_child(Parent, Child) ->
    service_supervisor:add_child(Parent, Child).

handle_peer() ->
    mob_dht:peer().

-ifdef(TEST).
-compile([export_all]).
-endif.
