-module(kbucket).
-export([start/2]).
-export([loop/1]).
-export([put/2]).
-export([set_peer/2]).
-export([is_closest/3]).
-export([k_closest_to/3]).
-export([closest_contacts/2]).
-export([refresh/1]).
-export([refresh_bucket/3]).

-type contact() :: {pid(), integer()} .

-record(kbucket, {peer, k, contacts, keylength}).

start(K, Keylength) ->
    Kbucket = #kbucket{k = K, keylength = Keylength, contacts = #{}},
    spawn(fun() -> loop(Kbucket) end).

put(KbucketPid, Peer) ->
    KbucketPid ! {put, self(), Peer},
    ok.

get(KbucketPid, BucketIndex) ->
    KbucketPid ! {get, self(), BucketIndex},
    receive
        {KbucketPid, Bucket} -> Bucket
    end.

set_peer(KbucketPid, PeerContact) ->
    KbucketPid ! {set_peer, PeerContact},
    ok.

closest_contacts(KbucketPid, Key) ->
    KbucketPid ! {closest_contacts, self(), Key},
    receive
        {KbucketPid, Contacts} -> Contacts
    end.

k_closest_to(KbucketPid, Key, Contacts) ->
    KbucketPid ! {k_closest_to, self(), Key, Contacts},
    receive
        {KbucketPid, Result} ->
            Result
    end.

refresh(Kbucket) ->
    Kbucket ! {refresh, self()},
    receive
        {Kbucket, ok} ->
            ok
    end.

loop(#kbucket{keylength = Keylength, peer = Peer, contacts = Contacts} = Kbucket) ->
    receive
        {put, FromPeer, Contact} when Contact =/= Peer ->
            log:kbucket(Kbucket, [], "PUT ~p", [Contact]),
            NewKbucket = handle_put(Kbucket, Contact),
            loop(NewKbucket);
        {closest_contacts, FromPeer, Key} ->
            log:kbucket(Kbucket, log:pid_to_field(FromPeer, "from"),  "ClOSEST_CONTACTS ~p", [Key]),
            ClosestContacts = handle_closest_contacts(Kbucket, Key),
            FromPeer ! {self(), ClosestContacts},
            loop(Kbucket);
        {get, FromPeer, BucketIndex} ->
            FromPeer ! {self(), bucket(BucketIndex, Contacts)},
            loop(Kbucket);
        {set_peer, PeerContact} ->
            NewKbucket = Kbucket#kbucket{peer = PeerContact},
            loop(NewKbucket);
        {k_closest_to, From, Key, ListOfContacts} ->
            log:kbucket(Kbucket, log:pid_to_field(From, "from"), "K_CLOSEST_TO ~p ~p", [Key, ListOfContacts]),
            Result = handle_k_closest_to(Kbucket, Key, ListOfContacts),
            From ! {self(), Result},
            loop(Kbucket);
        {refresh, From} ->
            log:kbucket(Kbucket, log:pid_to_field(From, "from"),  "REFRESH"),
            handle_refresh(Kbucket, Keylength),
            From ! {self(), ok},
            loop(Kbucket);
        _ ->
            loop(Kbucket)
    end.

handle_k_closest_to(#kbucket{k = K}, Key, ListOfContacts) ->
    SortedContacts = sort_on(Key, ListOfContacts),
    lists:sublist(SortedContacts, K).

handle_refresh(#kbucket{peer = Peer, contacts = Contacts}, Keylength) ->
    StartingBucket = first_occupied_bucket(Contacts) + 1,
    EndBucket = Keylength - 1,
    lists:foreach(fun(Index) ->
                    spawn(kbucket, refresh_bucket, [self(), Index, Peer])
                  end, lists:seq(StartingBucket, EndBucket)).

handle_closest_contacts(#kbucket{k = K, contacts = Contacts}, Key) ->
    SortedContacts = sort_on(Key, contacts_to_list(Contacts)),
    lists:sublist(SortedContacts, K).

handle_put(#kbucket{contacts = Contacts} = Kbucket, Contact) ->
    {BucketIndex, Bucket} = bucket_for(Kbucket, Contact),
    NewBucket = put_on(Bucket, Contact, Kbucket),
    NewContacts = Contacts#{BucketIndex => NewBucket},
    Kbucket#kbucket{contacts = NewContacts}.

bucket_for(#kbucket{contacts = Contacts, peer = {_, MyId}}, {_, ContactId}) ->
    DestinationBucketIndex = bucket_index(distance(MyId, ContactId)),
    {DestinationBucketIndex, bucket(DestinationBucketIndex, Contacts)}.

put_on([LeastContact | PartialBucket] = Bucket, LeastContact, #kbucket{k = K}) when length(Bucket) =:= K ->
    PartialBucket ++ [LeastContact];
put_on([LeastContact | PartialBucket] = Bucket, Contact, #kbucket{k = K, peer = Peer}) when length(Bucket) =:= K ->
    case peer:check_link(LeastContact, Peer) of
        ok -> Bucket;
        ko -> put_on(PartialBucket, Contact, K)
    end;
put_on(Bucket, Contact, _) ->
    CleanedBucket = lists:delete(Contact, Bucket),
    lists:append(CleanedBucket, [Contact]).

refresh_bucket(Kbucket, BucketIndex, {_, PeerId} = Peer) ->
    Key = gen_key_within(BucketIndex, PeerId),
    ClosestPeers = peer:iterative_find_peers(Peer, Key),
    [kbucket:put(Kbucket, Contact) || Contact <- ClosestPeers].

bucket(BucketIndex, Contacts) ->
    case maps:is_key(BucketIndex, Contacts) of
        true -> #{BucketIndex := Bucket} = Contacts,
                Bucket;
        _    -> []
    end.

contacts_to_list(Contacts) ->
    AllContacts = lists:map(fun(Index) -> bucket(Index, Contacts) end, maps:keys(Contacts)),
    lists:flatten(AllContacts).

sort_on(Key, Contacts) ->
    lists:sort(fun(PeerA, PeerB) -> is_closest(PeerA, PeerB, Key) end, Contacts).

distance(FromPeerId, ToPeerId) ->
    FromPeerId bxor ToPeerId.

is_closest({_, PeerAId}, {_, PeerBId}, Key) ->
    distance(Key, PeerAId) < distance(Key, PeerBId).

bucket_index(Distance) ->
    trunc(math:log2(Distance)).

gen_key_within(BucketIndex, Id) ->
    trunc(math:pow(2, BucketIndex)) bxor Id.

first_occupied_bucket(Contacts) ->
    Indices = maps:keys(Contacts),
    case length(Indices) =:= 0 of
        true  -> -1;
        false ->
            NotEmptyBuckets = lists:dropwhile(fun(I) -> length(bucket(I, Contacts)) =:= 0 end,
                                              maps:keys(Contacts)),
            hd(NotEmptyBuckets)
    end.

-ifdef(TEST).
-compile([export_all]).
-endif.
