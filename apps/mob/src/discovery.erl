-module(discovery).

-export([merge_nodes/2]).
-export([find_available_node/2]).

-include("service.hrl").

merge_nodes(PeerA, PeerB) ->
    %% XXX: this "should" never fail since every node annunce itself
    {found, ANodes} = peer:iterative_find_value(PeerA, nodes),
    {found, BNodes} = peer:iterative_find_value(PeerB, nodes),
    sets:union(ANodes, BNodes).

find_available_node(Peer, _Service) ->
    case peer:iterative_find_value(Peer, nodes) of
        {found, Nodes} ->
            {ok, hd(sets:to_list(Nodes))};
        _ ->
            {error, no_nodes}
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/discovery.hrl").
-endif.
