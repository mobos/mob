-module(discovery).

-export([merge_key_sets/3]).
-export([find_available_node/2]).
-export([announce_nodes/2]).
-export([announce_providers/2]).
-export([announce_spawned_service/3]).
-export([where_deployed/2]).
-export([init_net/3]).
-export([services/1]).

%% XXX discovery should doesn't know service
%% details
-include("service_supervisor/service.hrl").

-define(NODES_KEY, nodes).
-define(SERVICES_KEY, services).

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

init_net(Peer, Node, Providers) ->
    NodeSet = sets:add_element(Node, sets:new()),
    announce_providers(Peer, [{ProviderName, NodeSet} || ProviderName <- Providers]),
    peer:iterative_store(Peer, {?SERVICES_KEY, sets:new()}).

services(Peer) ->
    {found, Services} = peer:iterative_find_value(Peer, ?SERVICES_KEY),
    sets:to_list(Services).

find_available_node(Peer, _Service) ->
    case peer:iterative_find_value(Peer, ?NODES_KEY) of
        {found, Nodes} ->
            {ok, random_pick(Nodes)};
        _ ->
            {error, no_nodes}
    end.

announce_providers(_Peer, []) -> ok;
announce_providers(Peer, [{ProviderName, Nodes} | Providers]) ->
    peer:iterative_store(Peer, {ProviderName, Nodes}),
    announce_providers(Peer, Providers).

announce_nodes(Peer, UpdatedNodes) ->
    peer:iterative_store(Peer, {?NODES_KEY, UpdatedNodes}).

announce_spawned_service(Peer, Service, OwningNode) ->
    ServiceName = Service#service.name,
    peer:iterative_store(Peer, {ServiceName, OwningNode}),
    %% XXX: this "should" never fail
    {found, Services} = peer:iterative_find_value(Peer, services),
    UpdatedServices = sets:add_element(Service, Services),
    peer:iterative_store(Peer, {?SERVICES_KEY, UpdatedServices}).

where_deployed(Peer, ServiceName) ->
    case peer:iterative_find_value(Peer, ServiceName) of
        {found, Node} -> {found, Node};
        _ -> {error, not_found}
    end.

random_pick(Set) ->
    List = sets:to_list(Set),
    lists:nth(random:uniform(length(List)), List).

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/discovery.hrl").
-endif.
