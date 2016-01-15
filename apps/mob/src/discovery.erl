-module(discovery).

-export([merge_nodes/2]).
-export([find_available_node/2]).
-export([announce_nodes/2]).

-include("service.hrl").

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

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/discovery.hrl").
-endif.
