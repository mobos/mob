-module(mob_dht).

-behaviour(gen_server).

-export([start_link/1]).
-export([find_available_node/1]).
-export([announce_spawned_service/2]).
-export([where_deployed/1]).
-export([join/1]).
-export([services/0]).
-export([peer/0]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

%% XXX mob_dht should doesn't know service
%% details
-include("service_supervisor/service.hrl").

-define(KNOWN_PROVIDERS, [bash, docker]).

-define(SERVICES_KEY, services).
-define(NODE_NAME, node()).

-record(state, {peer}).

start_link(Args) ->
    gen_server:start_link({local, mob_dht}, ?MODULE, [Args], []).

find_available_node(Service) ->
    gen_server:call(mob_dht, {find_available_node, Service}).

where_deployed(ServiceName) ->
    gen_server:call(mob_dht, {where_deployed, ServiceName}).

join(NodeName) ->
    gen_server:call(mob_dht, {join, NodeName}).

services() ->
    gen_server:call(mob_dht, {services}).

announce_spawned_service(Service, NodeName) ->
    gen_server:call(mob_dht, {announce_spawned_service, Service, NodeName}).

peer() ->
    gen_server:call(mob_dht, {peer}).

%% Callbacks
init([Args]) ->
    Peer = init_peer(?NODE_NAME),
    Providers = args_utils:get_as_atom(providers, Args),
    init_net(Peer, ?NODE_NAME, Providers),
    {ok, #state{peer = Peer}}.

handle_call({peer}, _From, State = #state{peer = Peer}) ->
    Reply = Peer,
    {reply, Reply, State};
handle_call({services}, _From, State = #state{peer = Peer}) ->
    Reply = handle_services(Peer),
    {reply, Reply, State};
handle_call({announce_spawned_service, Service, NodeName}, _From, State = #state{peer = Peer}) ->
    Reply = handle_announce_spawned_service(Service, NodeName, Peer),
    {reply, Reply, State};
handle_call({join, NodeName}, _From, State = #state{peer = Peer}) ->
    Reply = handle_join(NodeName, Peer),
    {reply, Reply, State};
handle_call({where_deployed, ServiceName}, _From, State = #state{peer = Peer}) ->
    Reply = handle_where_deployed(ServiceName, Peer),
    {reply, Reply, State};
handle_call({find_available_node, Service}, _From, State = #state{peer = Peer}) ->
    Reply = handle_find_available_node(Service, Peer),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Handlers

handle_join(NodeName, Peer) ->
    ConnectionResult = remote_mob:connect(NodeName),
    BootstrapPeer = remote_mob:peer(NodeName),

    UpdatedProviders = merge_key_sets(Peer, BootstrapPeer, ?KNOWN_PROVIDERS),
    peer:join(Peer, BootstrapPeer),

    %% XXX: here should we wait to be sure that the joining process is
    %% completed ?
    announce_providers(Peer, UpdatedProviders),
    ConnectionResult.

init_net(Peer, Node, Providers) ->
    NodeSet = sets:add_element(Node, sets:new()),
    announce_providers(Peer, [{ProviderName, NodeSet} || ProviderName <- Providers]),
    peer:iterative_store(Peer, {?SERVICES_KEY, sets:new()}).

handle_services(Peer) ->
    {found, Services} = peer:iterative_find_value(Peer, ?SERVICES_KEY),
    Services.

handle_find_available_node(Service, Peer) ->
    case peer:iterative_find_value(Peer, Service#service.provider) of
        {found, Nodes} -> {ok, random_pick(Nodes)};
        _ ->              {error, no_nodes}
    end.

handle_announce_spawned_service(Service, NodeName, Peer) ->
    ServiceName = Service#service.name,
    AllServices = handle_services(Peer),
    UpdatedServices = sets:add_element(Service, AllServices),

    peer:iterative_store(Peer, {?SERVICES_KEY, UpdatedServices}),
    peer:iterative_store(Peer, {ServiceName, NodeName}).

handle_where_deployed(ServiceName, Peer) ->
    case peer:iterative_find_value(Peer, ServiceName) of
        {found, Node} -> {found, Node};
        _             -> {error, not_found}
    end.

random_pick(Set) ->
    List = sets:to_list(Set),
    lists:nth(random:uniform(length(List)), List).

init_peer(NodeName) ->
    {ok, PeerConf} = application:get_env(kademlia),

    {k, K} = lists:keyfind(k, 1, PeerConf),
    {alpha, Alpha} = lists:keyfind(alpha, 1, PeerConf),

    Id = peer:hash_key(NodeName),
    peer:start(Id, K, Alpha).

announce_providers(_Peer, []) -> ok;
announce_providers(Peer, [{ProviderName, Nodes} | Providers]) ->
    peer:iterative_store(Peer, {ProviderName, Nodes}),
    announce_providers(Peer, Providers).

get_key_set(Peer, Key) ->
    case peer:iterative_find_value(Peer, Key) of
    {found, Value} -> Value;
    _ -> sets:new()
    end.

merge_key_set(PeerA, PeerB, Key) ->
    SetA = get_key_set(PeerA, Key),
    SetB = get_key_set(PeerB, Key),
    sets:union(SetA, SetB).

merge_key_sets(PeerA, PeerB, Keys) ->
    [{Key, merge_key_set(PeerA, PeerB, Key)} || Key <- Keys].

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/mob_dht.hrl").
-endif.
