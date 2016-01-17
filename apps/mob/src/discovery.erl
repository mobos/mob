-module(discovery).

-export([merge_nodes/2]).
-export([find_available_node/2]).
-export([announce_nodes/2]).
-export([announce_spawned_service/3]).
-export([where_deployed/2]).

%% XXX discovery should doesn't know service
%% details
-include("service_supervisor/service.hrl").

-define(NODES_KEY, nodes).

merge_nodes(PeerA, PeerB) ->
    %% XXX: this "should" never fail since every node annunce itself
    {found, ANodes} = peer:iterative_find_value(PeerA, ?NODES_KEY),
    {found, BNodes} = peer:iterative_find_value(PeerB, ?NODES_KEY),
    sets:union(ANodes, BNodes).

find_available_node(Peer, _Service) ->
    case peer:iterative_find_value(Peer, ?NODES_KEY) of
        {found, Nodes} ->
            {ok, hd(sets:to_list(Nodes))};
        _ ->
            {error, no_nodes}
    end.

announce_nodes(Peer, UpdatedNodes) ->
    peer:iterative_store(Peer, {?NODES_KEY, UpdatedNodes}).

announce_spawned_service(Peer, Service, OwningNode) ->
    ServiceName = Service#service.name,
    peer:iterative_store(Peer, {ServiceName, OwningNode}).

where_deployed(Peer, Service) ->
    ServiceName = Service#service.name,
    case peer:iterative_find_value(Peer, ServiceName) of
        {found, Node} -> {found, Node};
        _ -> {error, not_found}
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/discovery.hrl").
-endif.
