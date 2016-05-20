-include_lib("eunit/include/eunit.hrl").
-include_lib("test_macro.hrl").

-define(PEER_ID, 13).
-define(ID_LENGTH, 4).
-define(ALPHA, 3).

start() ->
    meck:new(peer),
    meck:expect(peer, start, fun(Id, Kbucket, Alpha) -> {self(), Id} end),
    K = 3,
    KbucketPid = kbucket:start(K, ?ID_LENGTH),
    Peer = peer:start(?PEER_ID, KbucketPid, ?ALPHA),
    kbucket:set_peer(KbucketPid, Peer),
    {KbucketPid, Peer}.

teardown(_) ->
    meck:unload(peer).

peer_suite_test_() ->
    [?setup(fun should_start_a_kbucket_process/1),
     ?setup(fun should_create_a_bucket_if_not_exists/1),
     ?setup(fun should_append_a_contacts_with_the_same_bucket_index/1),
     ?setup(fun should_update_an_already_present_contacts/1),
     ?setup(fun ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_respond/1),
     ?setup(fun ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_respond/1),
     ?setup(fun should_returns_an_empty_list_for_an_unknown_distance/1),
     ?setup(fun should_fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full/1),
     ?setup(fun should_returns_up_to_k_contacts_closest_to_a_key/1),
     ?setup(fun should_search_a_key_within_each_bucket_and_refresh_its_content/1)].

should_compute_the_distance_of_two_id_test() ->
    ?assertEqual(3, kbucket:distance(2#0001, 2#0010)).

should_returns_the_correct_bucket_index_of_a_distance_test() ->
    ?assertEqual(0, kbucket:bucket_index(1)),
    ?assertEqual(1, kbucket:bucket_index(2)),
    ?assertEqual(1, kbucket:bucket_index(3)),
    ?assertEqual(2, kbucket:bucket_index(4)),
    ?assertEqual(2, kbucket:bucket_index(5)),
    ?assertEqual(2, kbucket:bucket_index(6)),
    ?assertEqual(2, kbucket:bucket_index(7)),
    ?assertEqual(3, kbucket:bucket_index(8)),
    ?assertEqual(7, kbucket:bucket_index(130)).

should_start_a_kbucket_process({KbucketPid, _}) ->
    [?_assert(erlang:is_pid(KbucketPid))].

should_create_a_bucket_if_not_exists({KbucketPid, _}) ->
    PeerContact = {self(), 2#1111},

    ok = kbucket:put(KbucketPid, PeerContact),

    [?_assertEqual([PeerContact], kbucket:get(KbucketPid, 1))].

should_returns_an_empty_list_for_an_unknown_distance({KbucketPid, _}) ->
    [?_assertEqual([], kbucket:get(KbucketPid, 100))].

should_append_a_contacts_with_the_same_bucket_index({KbucketPid, _}) ->
    PeerContact    = {self(), 2#1111}, % distance of 2 from OwningId
    AnotherContact = {self(), 2#1110}, % distance of 3 from OwningId

    put_contacts(KbucketPid, [PeerContact, AnotherContact]),

    [?_assertEqual([PeerContact, AnotherContact], kbucket:get(KbucketPid, 1))].

should_update_an_already_present_contacts({KbucketPid, _}) ->
    PeerContact    = {self(), 2#1111},
    AnotherContact = {self(), 2#1110},
    put_contacts(KbucketPid, [PeerContact, AnotherContact]),

    ok = kbucket:put(KbucketPid, PeerContact),

    [?_assertEqual([AnotherContact, PeerContact], kbucket:get(KbucketPid, 1))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_respond({KbucketPid, _}) ->
    % these defines fakes contacts.
    % the IDs are such that its BucketIndex are the
    % same if stored in a peer with 2#1101 ID.
    FourPeerContact  = peer:start(2#1001, 3, ?ALPHA),
    FivePeerContact  = {self(), 2#1000},
    SixPeerContact   = {self(), 2#1011},
    SevenPeerContact = {self(), 2#1010},
    put_contacts(KbucketPid, [FourPeerContact, FivePeerContact, SixPeerContact]),
    meck:expect(peer, check_link,  ?two_any_args(?return(ko))),

    ok = kbucket:put(KbucketPid, SevenPeerContact),

    ExpectedBucket = [FivePeerContact, SixPeerContact, SevenPeerContact],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_respond({KbucketPid, _}) ->
    FourPeerContact  = peer:start(2#1001, 3, ?ALPHA),
    FivePeerContact  = {self(), 2#1000},
    SixPeerContact   = {self(), 2#1011},
    SevenPeerContact = {self(), 2#1010},
    put_contacts(KbucketPid, [FourPeerContact, FivePeerContact, SixPeerContact]),
    meck:expect(peer, check_link, ?two_any_args(?return(ok))),

    ok = kbucket:put(KbucketPid, SevenPeerContact),

    ExpectedBucket = [FourPeerContact, FivePeerContact, SixPeerContact],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2))].

should_returns_up_to_k_contacts_closest_to_a_key({KbucketPid, _}) ->
    ThreePeerContact  = {self(), 2#1001},
    TwoPeerContact    = {self(), 2#1000},
    OnePeerContact    = {self(), 2#1011},
    put_contacts(KbucketPid, [OnePeerContact, TwoPeerContact, ThreePeerContact]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1010),

    [?_assertEqual([OnePeerContact, TwoPeerContact, ThreePeerContact], ClosestContacts)].

should_fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full({KbucketPid, _}) ->
    FivePeerContact   = {self(), 2#1111},
    ThreePeerContact  = {self(), 2#1001},
    TwoPeerContact    = {self(), 2#1000},
    put_contacts(KbucketPid, [TwoPeerContact, ThreePeerContact, FivePeerContact]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1010),

    [?_assertEqual([TwoPeerContact, ThreePeerContact, FivePeerContact], ClosestContacts)].

should_search_a_key_within_each_bucket_and_refresh_its_content({KbucketPid, Peer}) ->
    ZeroBucketContact  = {self(), 12},
    OneBucketContact   = {self(), 15},
    TwoBucketContact   = {self(),  9},
    ThreeBucketContact = {self(),  5},
    AllContacts = [ZeroBucketContact, OneBucketContact, TwoBucketContact, ThreeBucketContact],
    meck:expect(peer, iterative_find_peers, fun(_, Key) ->
                                                case Key of
                                                    12 -> AllContacts;
                                                    15 -> AllContacts;
                                                     9 -> AllContacts;
                                                     5 -> AllContacts
                                                end
                                            end),

    kbucket:refresh(KbucketPid),

    timer:sleep(50),
    [?_assertEqual([ZeroBucketContact],  kbucket:get(KbucketPid, 0)),
     ?_assertEqual([OneBucketContact],   kbucket:get(KbucketPid, 1)),
     ?_assertEqual([TwoBucketContact],   kbucket:get(KbucketPid, 2)),
     ?_assertEqual([ThreeBucketContact], kbucket:get(KbucketPid, 3))].

put_contacts(KbucketPid, []) ->
    ok;
put_contacts(KbucketPid, [Contact | Contacts]) ->
    ok = kbucket:put(KbucketPid, Contact),
    put_contacts(KbucketPid, Contacts).
