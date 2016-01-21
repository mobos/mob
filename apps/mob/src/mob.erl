-module(mob).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([join/1]).
-export([run/1]).
-export([remotely_restart/1]).
-export([remotely_add_child/2]).
-export([deploy/1]).
-export([is_remotely_started/1]).
-export([node_name/0]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-include("service_supervisor/service.hrl").

-record(state, {peer, providers}).

-define(KNOWN_PROVIDERS, [bash]).

start_link(Args) ->
    gen_server:start_link({local, mob}, ?MODULE, [Args], []).

deploy(Service) ->
    gen_server:call(mob, {deploy, Service}).

run(Service) ->
    gen_server:cast(mob, {run, Service}).

join(NodeName) ->
    gen_server:call(mob, {join, NodeName}).

node_name() ->
    node().

is_remotely_started(ServiceName) ->
    gen_server:call(mob, {is_remotely_started, ServiceName}).

remotely_restart(ServiceName) ->
    gen_server:call(mob, {remotely_restart, ServiceName}).

remotely_add_child(ParentName, ChildName) ->
    gen_server:call(mob, {remotely_add_child, ParentName, ChildName}).

%% Callbacks

init_peer() ->
    {ok, PeerConf} = application:get_env(kademlia),

    {k, K} = lists:keyfind(k, 1, PeerConf),
    {alpha, Alpha} = lists:keyfind(alpha, 1, PeerConf),

    Id = peer:hash_key(node_name()),
    peer:start(Id, K, Alpha).

init([Args]) ->
    Peer = init_peer(),
    Providers = args_utils:get_as_atom(providers, Args),
    discovery:init_net(Peer, node_name(), Providers),
    {ok, #state{peer = Peer, providers = Providers}}.

handle_call(peer, _From, State = #state{peer = Peer}) ->
    {reply, Peer, State};

handle_call({join, NodeName}, _From, State) ->
    {Reply, State} = handle_join(NodeName, State),
    {reply, Reply, State};

handle_call({deploy, Service}, _From, State) ->
    {Reply, NewState} = handle_deploy(Service, State),
    {reply, Reply, NewState};

handle_call({is_started, ServiceName}, _From, State) ->
    {Reply, NewState} = handle_is_started(ServiceName, State),
    {reply, Reply, NewState};

handle_call({is_remotely_started, Service}, _From, State) ->
    {Reply, NewState} = handle_is_remotely_started(Service, State),
    {reply, Reply, NewState};

handle_call({remotely_restart, Service}, _From, State) ->
    {Reply, NewState} = handle_remotely_restart(Service, State),
    {reply, Reply, NewState};

handle_call({remotely_add_child, Parent, Child}, _From, State) ->
    {Reply, NewState} = handle_remotely_add_child(Parent, Child, State),
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

handle_join(NodeName, State = #state{peer = Peer}) ->
    ConnectionResult = remote_mob:connect(NodeName),
    BootstrapPeer = remote_mob:peer(NodeName),

    UpdatedProviders = discovery:merge_key_sets(Peer, BootstrapPeer, ?KNOWN_PROVIDERS),
    peer:join(Peer, BootstrapPeer),

    %% XXX: here should we wait to be sure that the joining process is
    %% completed ?
    discovery:announce_providers(Peer, UpdatedProviders),
    {ConnectionResult, State}.

handle_deploy(Service, State) ->
    Reply = case service_parser:parse(Service) of
                {ok, ParsedService} -> do_deploy(ParsedService, State);
                {error, Error} -> Error
            end,
    {Reply, State}.

handle_run(Service, State = #state{peer = Peer}) ->
    %% XXX Currently the service is annunced *BEFORE* the service
    %% is really spawned. To be sure we should notify this module
    %% when the service is really spawned and proceed with the
    %% announce
    log:notice("[~p] Run request for ~p", [?MODULE, Service#service.name]),
    Services = discovery:services(Peer),
    service_supervisor:run(Service, Services),
    discovery:announce_spawned_service(Peer, Service, node_name()),
    State.

handle_is_started(ServiceName, State) ->
    Reply = service_supervisor:is_started(ServiceName),
    {Reply, State}.

handle_restart(ServiceName, State) ->
    service_supervisor:restart(ServiceName),
    State.

handle_is_remotely_started(ServiceName, State = #state{peer = Peer}) ->
    Ret = case discovery:where_deployed(Peer, ServiceName) of
        {found, Node} -> remote_mob:is_started(Node, ServiceName);
        _ -> false
    end,
    {Ret, State}.

handle_remotely_restart(ServiceName, State = #state{peer = Peer}) ->
    Ret = case discovery:where_deployed(Peer, ServiceName) of
              {found, Node} -> remote_mob:restart(Node, ServiceName);
              _ -> not_found
          end,
    {Ret, State}.

handle_remotely_add_child(Parent, Child, State = #state{peer = Peer}) ->
   Ret = case discovery:where_deployed(Peer, Parent) of
             {found, Node} -> remote_mob:add_child(Node, Parent, Child);
             _ -> not_found
         end,
   {Ret, State}.

handle_add_child(Parent, Child, State) ->
    service_supervisor:add_child(Parent, Child),
    State.

do_deploy(ParsedService, #state{peer = Peer}) ->
    case discovery:where_deployed(Peer, ParsedService#service.name) of
        {error, not_found} ->
            case discovery:find_available_node(Peer, ParsedService) of
                {ok, Node} ->
                    remote_mob:run(Node, ParsedService),
                    Node;
                {error, Error} -> Error
            end;
        {found, _Node} -> already_deployed
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/mob.hrl").
-endif.
