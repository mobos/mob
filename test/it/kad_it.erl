-module(kad_it).

-include_lib("test/test_macro.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ALPHA, 3).
-define(K, 3).

should_find_the_closest_peer_for_a_given_key_test() ->
     ClosestKey = 2#0000,
     Peer        = peer:start(2#1000, ?K, ?ALPHA),
     MiddlePeer  = peer:start(2#0010, ?K, ?ALPHA),
     ClosestPeer = peer:start(2#0001, ?K, ?ALPHA),

     % check the links between peers to simulate a join
     %
     %    Peer <--> MiddlePeer <--> ClosestPeer
     %
     peer:check_link(Peer, MiddlePeer),
     peer:check_link(MiddlePeer, ClosestPeer),
     ?assertEqual([ClosestPeer, MiddlePeer, Peer],
                  peer:iterative_find_peers(Peer, ClosestKey)).

should_find_the_closest_peers_not_directly_connected_test() ->
    %% since the searched key is 0, the distance of a peer
    %% from the key is equals to the ID of the peer
    ClosestKey = 2#0000,
    ClosestA = peer:start(2#0001, ?K, ?ALPHA),
    ClosestB = peer:start(2#0010, ?K, ?ALPHA),
    ClosestC = peer:start(2#0011, ?K, ?ALPHA),

    LocalClosest = peer:start(2#0100, ?K, ?ALPHA),
    LocalAway = peer:start(2#1111, ?K, ?ALPHA),

    Peer = peer:start(2#1000, ?K, ?ALPHA),

    %%       LocalAway
    %%     /
    %%  Peer              / ClosestA
    %%     \ LocalClosest - ClosestB
    %%                    \ ClosestC
    peer:check_link(Peer, LocalAway),
    peer:check_link(Peer, LocalClosest),

    peer:check_link(LocalClosest, ClosestA),
    peer:check_link(LocalClosest, ClosestB),
    peer:check_link(LocalClosest, ClosestC),

    ?assertEqual([ClosestA, ClosestB, ClosestC],
                 peer:iterative_find_peers(Peer, ClosestKey)).

should_find_the_closest_peers_on_different_branch_not_directly_connetted_test() ->
    %% since the searched key is 0, the distance of a peer
    %% from the key is equals to the ID of the peer
    ClosestKey = 2#0000,
    ClosestA = peer:start(2#0001, ?K, ?ALPHA),
    ClosestB = peer:start(2#0010, ?K, ?ALPHA),
    ClosestC = peer:start(2#0011, ?K, ?ALPHA),

    LocalClosest = peer:start(2#0100, ?K, ?ALPHA),
    LocalAway = peer:start(2#1111, ?K, ?ALPHA),
    PeerAway = peer:start(2#1100, ?K, ?ALPHA),

    Peer = peer:start(2#1000, ?K, ?ALPHA),

    %%       LocalAway - ClosestA
    %%     /
    %%  Peer              / PeerAway
    %%     \ LocalClosest - ClosestB
    %%                    \ ClosestC
    peer:check_link(Peer, LocalAway),
    peer:check_link(Peer, LocalClosest),

    peer:check_link(LocalClosest, PeerAway),
    peer:check_link(LocalClosest, ClosestB),
    peer:check_link(LocalClosest, ClosestC),

    peer:check_link(LocalAway, ClosestA),

    ?assertEqual([ClosestA, ClosestB, ClosestC],
                 peer:iterative_find_peers(Peer, ClosestKey)).

should_find_the_closest_peers_also_with_its_directly_connected_peers_test() ->
    ClosestKey = 2#0000,
    ClosestA = peer:start(2#0001, ?K, ?ALPHA),
    ClosestB = peer:start(2#0010, ?K, ?ALPHA),

    LocalClosest = peer:start(2#0100, ?K, ?ALPHA),
    LocalAway = peer:start(2#1111, ?K, ?ALPHA),
    PeerAway = peer:start(2#1100, ?K, ?ALPHA),

    Peer = peer:start(2#1000, ?K, ?ALPHA),

    %%       LocalAway - ClosestA
    %%     /
    %%  Peer              / PeerAway
    %%     \ LocalClosest
    %%                    \ ClosestB
    peer:check_link(Peer, LocalAway),
    peer:check_link(Peer, LocalClosest),

    peer:check_link(LocalClosest, PeerAway),
    peer:check_link(LocalClosest, ClosestB),

    peer:check_link(LocalAway, ClosestA),

    ?assertEqual([ClosestA, ClosestB, LocalClosest],
                 peer:iterative_find_peers(Peer, ClosestKey)).

a_find_should_not_return_a_peer_that_goes_in_timeout_test() ->
    ClosestKey = 2#0000,
    ClosestDown = peer:start(2#0001, ?K, ?ALPHA),
    ClosestB = peer:start(2#0010, ?K, ?ALPHA),
    ClosestC = peer:start(2#0011, ?K, ?ALPHA),

    LocalClosest = peer:start(2#0100, ?K, ?ALPHA),
    LocalAway = peer:start(2#1111, ?K, ?ALPHA),

    Peer = peer:start(2#1000, ?K, ?ALPHA),

    %%       LocalAway
    %%     /
    %%  Peer              / ClosestDown
    %%     \ LocalClosest - ClosestB
    %%                    \ ClosestC
    peer:check_link(Peer, LocalAway),
    peer:check_link(Peer, LocalClosest),

    peer:check_link(LocalClosest, ClosestDown),
    peer:check_link(LocalClosest, ClosestB),
    peer:check_link(LocalClosest, ClosestC),

    shutdown_peer(ClosestDown),

    ?assertEqual([ClosestB, ClosestC, LocalClosest],
                 peer:iterative_find_peers(Peer, ClosestKey)).

should_find_closest_peers_with_more_than_alpha_local_peer_test() ->
    K = 4,

    ClosestKey = 2#0000,
    ClosestA = peer:start(2#0001, K, ?ALPHA),

    LocalClosestA = peer:start(2#0010, K, ?ALPHA),
    LocalClosestB = peer:start(2#0011, K, ?ALPHA),
    LocalClosestC = peer:start(2#0100, K, ?ALPHA),
    LocalAway = peer:start(2#1111, K, ?ALPHA),

    Peer = peer:start(2#1000, K, ?ALPHA),

    %%      + LocalClosestA
    %%      |
    %%  Peer+ LocalAway - ClosestA
    %%      |
    %%      + LocalClosestB
    %%      |
    %%      + LocalClosestC
    %%
    %% The search starts with best-active [Peer] and Knowlege
    %% [LCA, LCB, LCC, LA] Since alpha = 3 it picks alpha-best
    %% one: [LCA, LCB, LCC] and iterate again.
    %% The new result are, best-active [LCA, LCB, LCC, Peer]
    %% and knowlege [*, *, *, LA]; since LA is far away from LCA
    %% the "normal" find should stops.
    %% But, such as from the paper, in this situation send a
    %% find_* to all uncontacted peers so we can find CLA.
    peer:check_link(Peer, LocalAway),
    peer:check_link(Peer, LocalClosestA),
    peer:check_link(Peer, LocalClosestB),
    peer:check_link(Peer, LocalClosestC),

    peer:check_link(LocalAway, ClosestA),

    ?assertEqual([ClosestA, LocalClosestA, LocalClosestB, LocalClosestC],
                 peer:iterative_find_peers(Peer, ClosestKey)).

join_should_update_kbucket_of_bootstrap_peer_test() ->
    {KbucketA, PeerA} = new_peer(1, ?K, ?ALPHA),
    {KbucketB, PeerB} = new_peer(2, ?K, ?ALPHA),

    peer:join(PeerA, PeerB),

    ?assertEqual([PeerB], kbucket:get(KbucketA, 1)),
    ?assertEqual([PeerA], kbucket:get(KbucketB, 1)).

 a_joining_peer_should_know_its_closest_neighbours_test() ->
     K = 3,
     Alpha = 3,
     {KbucketA, PeerA} = new_peer(1, K, Alpha),
     {KbucketB, PeerB} = new_peer(9, K, Alpha),
     {KbucketC, PeerC} = new_peer(10, K, Alpha),
     {KbucketD, PeerD} = new_peer(2, K, Alpha),

     peer:join(PeerA, PeerB),
     peer:join(PeerC, PeerB),
     peer:join(PeerD, PeerC),

     timer:sleep(100),
     ?assertEqual([PeerD],        kbucket:get(KbucketA, 1)),
     ?assertEqual([PeerB, PeerC], kbucket:get(KbucketA, 3)),

     ?assertEqual([PeerC],        kbucket:get(KbucketB, 1)),
     ?assertEqual([PeerA, PeerD], kbucket:get(KbucketB, 3)),

     ?assertEqual([PeerB],        kbucket:get(KbucketC, 1)),
     ?assertEqual([PeerA, PeerD], kbucket:get(KbucketC, 3)),

     ?assertEqual([PeerA],        kbucket:get(KbucketD, 1)),
     ?assertEqual([PeerC, PeerB], kbucket:get(KbucketD, 3)).

should_store_a_key_on_closest_peers_test() ->
    K = 3,
    Alpha = 3,
    FakePeer = {self(), 100},

    Key     = key,
    HashKey = 16#A62F2225BF70BFACCBC7F1EF2A397836717377DE,
    Value   = "value",

    % distance with HashKey
    % PeerA = 4, PeerB = 7, PeerC = 14, PeerD = ~2^159
    PeerA = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377DA, K, Alpha),
    PeerB = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D9, K, Alpha),
    PeerC = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D0, K, Alpha),
    PeerD = peer:start(16#F62F2225BF70BFACCBC7F1EF2A397836717377DE, K, Alpha),

    peer:join(PeerA, PeerC),
    timer:sleep(50),
    peer:join(PeerB, PeerC),
    timer:sleep(50),
    peer:join(PeerD, PeerB),
    timer:sleep(50),

    peer:iterative_store(PeerA, {Key, Value}),
    timer:sleep(50),

    peer:find_value_of(PeerA, HashKey, FakePeer),
    ?receiving({PeerA, ResponseA}, ?assertEqual({found, Value}, ResponseA)),

    peer:find_value_of(PeerC, HashKey, FakePeer),
    ?receiving({PeerC, ResponseB}, ?assertEqual({found, Value}, ResponseB)),

    peer:find_value_of(PeerB, HashKey, FakePeer),
    ?receiving({PeerB, ResponseC}, ?assertEqual({found, Value}, ResponseC)),

    peer:find_value_of(PeerD, HashKey, FakePeer),
    ?receiving({PeerD, ResponseD}, ?assertEqual([PeerA, PeerB, PeerC], ResponseD)).

should_find_a_value_stored_in_itself_test() ->
    Key     = key,
    Value   = "value",

    PeerA = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377DA, ?K, ?ALPHA),
    peer:iterative_store(PeerA, {Key, Value}),

    timer:sleep(50),
    ?assertEqual({found, Value}, peer:iterative_find_value(PeerA, Key)).

should_find_a_value_stored_in_a_network_test() ->
    Key     = key,
    Value   = "value",

    % distance with HashKey
    % PeerA = 4, PeerB = 7, PeerC = 14, PeerD = ~2^159
    PeerA = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377DA, ?K, ?ALPHA),
    PeerB = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D9, ?K, ?ALPHA),
    PeerC = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D0, ?K, ?ALPHA),
    PeerD = peer:start(16#F62F2225BF70BFACCBC7F1EF2A397836717377DE, ?K, ?ALPHA),

    peer:join(PeerA, PeerC),
    timer:sleep(50),
    peer:join(PeerB, PeerC),
    timer:sleep(50),
    peer:join(PeerD, PeerB),
    timer:sleep(50),

    peer:iterative_store(PeerA, {Key, Value}),
    timer:sleep(50),

    ?assertEqual({found, Value}, peer:iterative_find_value(PeerD, Key)).

new_peer(Id, K, Alpha) ->
    Kbucket = kbucket:start(K, 4),
    Peer = peer:start(Id, Kbucket, Alpha),
    Kbucket ! {set_peer, Peer},
    {Kbucket, Peer}.

shutdown_peer({Pid, _}) ->
    exit(Pid, "shutting down"),
    ok.
