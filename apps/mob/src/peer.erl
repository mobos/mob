-module(peer).

-export([start/3]).
-export([loop/1]).
-export([ping/2]).
-export([pong/2]).
-export([iterative_find_peers/2]).
-export([iterative_store/2]).
-export([iterative_find_value/2]).
-export([find_value_of/3]).
-export([find_closest_peers/3]).
-export([check_link/2]).
-export([join/2]).
-export([store/3]).

-include("peer.hrl").
-define (TIMEOUT_REQUEST, 500).
-define (KEY_LENGTH, 160).


start(Id, KbucketPid, Alpha) when is_pid(KbucketPid) ->
    PeerPid = spawn(fun() -> loop(build_peer(Id, KbucketPid, Alpha)) end),
    {PeerPid, Id};
start(Id, K, Alpha) ->
    KbucketPid = kbucket:start(K, ?KEY_LENGTH),
    PeerPid = spawn(fun() -> loop(build_peer(Id, KbucketPid, Alpha)) end),
    PeerContact = {PeerPid, Id},
    KbucketPid ! {set_peer, PeerContact},
    PeerContact.

build_peer(Id, Kbucket, Alpha) ->
    {ok, RpcHandler} = gen_event:start(),
    {ok, Repository} = gen_server:start(repository, [], []),

    Peer = #peer{repository = Repository,
                 kbucket = Kbucket,
                 mycontact = {self(), Id},
                 alpha = Alpha,
                 rpc_handler = RpcHandler},

    gen_event:add_handler(RpcHandler, rpc_handler, Peer),
    Peer.

iterative_find_peers({PeerPid, _} = Peer, Key) ->
    PeerPid ! {iterative_find_peers, self(), Key},
    receive
        {Peer, Result} ->
            Result
    end.

iterative_store({PeerPid, _} = Peer, {Key, Value}) ->
    PeerPid ! {iterative_store, self(), {Key, Value}},
    receive
        {Peer, Result} ->
            Result
    end.

iterative_find_value({PeerPid, _} = Peer, Key) ->
    PeerPid ! {iterative_find_value, self(), Key},
    receive
        {Peer, Value} ->
            Value
    end.

check_link({PeerPid, _} = Peer, WithPeer) ->
    PeerPid ! {check_link, self(), WithPeer},
    receive
        {Peer, ok} ->
            ok;
        {Peer, ko} ->
            ko
    end.

store(Peer, {Key, Value}, FromPeer) ->
    call_rpc(Peer, store, FromPeer, [{Key, Value}]),
    ok.

find_closest_peers(Peer, Key, FromPeer) ->
    call_rpc(Peer, find_closest_peers, FromPeer, [Key]),
    ok.

find_value_of(Peer, Key, FromPeer) ->
    call_rpc(Peer, find_value, FromPeer, [Key]),
    ok.

ping(Peer, FromPeer) ->
    call_rpc(Peer, ping, FromPeer, []),
    ok.

pong({PeerPid, _}, FromPeer) ->
    PeerPid ! {rpc, pong, self(), FromPeer, []},
    ok.

join({PeerPid, _} = Peer, BootstrapPeer) ->
    PeerPid ! {join, self(), BootstrapPeer},
    receive
        {Peer, ok} ->
            ok
    end.

call_rpc({ToPeerPid, _}, RpcName, FromContact, Args) ->
    ToPeerPid ! {rpc, RpcName, self(), FromContact, Args}.

loop(#peer{kbucket = Kbucket, mycontact = MyContact, rpc_handler = RpcHandler} = Peer) ->
    receive
        {rpc, RpcName, FromPid, FromContact, Args} ->
            log:peer(Peer, log:contact_to_field(FromContact, "from_contact") ++
                           log:pid_to_field(FromPid, "from"), "~p ~p", [RpcName, Args]),

            kbucket:put(Kbucket, FromContact),
            ok = gen_event:notify(RpcHandler, {RpcName, FromPid, FromContact, Args}),
            loop(Peer);
        {check_link, From, ToContact} ->
            log:peer(Peer, log:contact_to_field(ToContact, "to") ++
                           log:pid_to_field(From, "from"), "CHECK_LINK ~p",[self()]),

            Result = handle_check_link(Peer, MyContact, ToContact),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_find_peers, From, Key} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "ITERATIVE_FIND_PEERS ~p", [Key]),
            Result = handle_iterative_find_peers(Peer, Key),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_store, From, {Key, Value}} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "ITERATIVE_STORE ~p ~p", [Key, Value]),
            HashedKey = hash_key(Key),
            handle_iterative_store(Peer, {HashedKey, Value}),
            From ! {MyContact, ok},
            loop(Peer);
        {iterative_find_value, From, Key} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "ITERATIVE_FIND_VALUE ~p", [Key]),
            Value = handle_iterative_find_value(Peer, hash_key(Key)),
            From ! {MyContact, Value},
            loop(Peer);
        {join, From, BootstrapPeer} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "JOIN ~p", [BootstrapPeer]),
            kbucket:put(Kbucket, BootstrapPeer),
            handle_join(Peer, BootstrapPeer),
            From ! {MyContact, ok},
            loop(Peer);
        _ ->
            loop(Peer)
    end.

handle_iterative_store(#peer{mycontact = MyContact} = Peer, {Key, Value}) ->
    ClosestPeers = handle_iterative_find_peers(Peer, Key),
    lists:foreach(fun(Contact) -> peer:store(Contact, {Key, Value}, MyContact) end, ClosestPeers).

handle_join(#peer{kbucket = Kbucket, mycontact = MyContact} = Peer, BootstrapPeer) ->
    {_, Id} = MyContact,
    MyKClosest = peer:iterative_find_peers(BootstrapPeer, Id),
    lists:foreach(fun(Neighbor) ->
                      peer:ping(Neighbor, MyContact)
                  end, lists:delete(MyContact, MyKClosest)),
    kbucket:refresh(Kbucket).

handle_check_link(Peer, MyContact, ToContact) ->
    ping(ToContact, MyContact),
    receive
        {rpc, pong, _FromPid, _ToContact, []} ->
            peer:pong(MyContact, ToContact),
            ok
    after ?TIMEOUT_REQUEST ->
            ko
    end.

handle_iterative_find_peers(#peer{kbucket = Kbucket, mycontact = MyContact, alpha = Alpha}, Key) ->
    network:find_peers(MyContact, Kbucket, Key, Alpha).

handle_iterative_find_value(#peer{kbucket = Kbucket, mycontact = MyContact, alpha = Alpha}, Key) ->
    network:find_value(MyContact, Kbucket, Key, Alpha).

hash_key(Key) ->
    HashedKey = crypto:hash(sha, atom_to_list(Key)),
    %% XXX: NumberKey should be removed when ID become binary
    <<NumberKey:?KEY_LENGTH, _/bitstring>> = HashedKey,
    NumberKey.

-ifdef(TEST).
-include_lib("../test/peer.hrl").
-endif.
