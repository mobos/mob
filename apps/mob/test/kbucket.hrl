-include_lib("eunit/include/eunit.hrl").
-include_lib("test_macro.hrl").

-define(PEER_ID, 13).
-define(ID_LENGTH, 4).
-define(ALPHA, 3).

% build a fake peer with the specified ID and fixed (dummy) PID
-define(FAKE_PEER(ID), {list_to_pid("<0.255.255>"), ID}).

% fake peers.
% the IDs are such that their Bucket Index are those specified
% on the prefix of the name (ZERO, ONE, ...) if stored in a
% peer with ID 2#1101 (?PEER_ID). On the same bucket index, the
% fake peers are sorted on distance (so _1 is closer than _2)
%
%                                           DISTANCE
-define(ZERO_PEER_1, ?FAKE_PEER(2#1100)).  % 1

-define(ONE_PEER_1,  ?FAKE_PEER(2#1111)).  % 2
-define(ONE_PEER_2,  ?FAKE_PEER(2#1110)).  % 3

-define(TWO_PEER_1,  ?FAKE_PEER(2#1001)).  % 4
-define(TWO_PEER_2,  ?FAKE_PEER(2#1000)).  % 5
-define(TWO_PEER_3,  ?FAKE_PEER(2#1011)).  % 6
-define(TWO_PEER_4,  ?FAKE_PEER(2#1010)).  % 7

-define(THIRD_PEER_1, ?FAKE_PEER(2#0101)). % 10

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
    ok = kbucket:put(KbucketPid, ?ONE_PEER_1),

    [?_assertEqual([?ONE_PEER_1], kbucket:get(KbucketPid, 1))].

should_returns_an_empty_list_for_an_unknown_distance({KbucketPid, _}) ->
    [?_assertEqual([], kbucket:get(KbucketPid, 100))].

should_append_a_contacts_with_the_same_bucket_index({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?ONE_PEER_1, ?ONE_PEER_2]),

    [?_assertEqual([?ONE_PEER_1, ?ONE_PEER_2], kbucket:get(KbucketPid, 1))].

should_update_an_already_present_contacts({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?ONE_PEER_1, ?ONE_PEER_2]),

    ok = kbucket:put(KbucketPid, ?ONE_PEER_1),

    [?_assertEqual([?ONE_PEER_2, ?ONE_PEER_1], kbucket:get(KbucketPid, 1))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_respond({KbucketPid, _}) ->
    meck:expect(peer, check_link,  ?two_any_args(?return(ko))),
    put_contacts(KbucketPid, [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3]),

    ok = kbucket:put(KbucketPid, ?TWO_PEER_4),

    ExpectedBucket = [?TWO_PEER_2, ?TWO_PEER_3, ?TWO_PEER_4],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_respond({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3]),
    meck:expect(peer, check_link, ?two_any_args(?return(ok))),

    ok = kbucket:put(KbucketPid, ?TWO_PEER_4),

    ExpectedBucket = [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2))].

should_returns_up_to_k_contacts_closest_to_a_key({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?TWO_PEER_3, ?TWO_PEER_2, ?TWO_PEER_1]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1101),

    [?_assertEqual([?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3], ClosestContacts)].

should_fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?TWO_PEER_2, ?TWO_PEER_1, ?ONE_PEER_1]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1010),

    [?_assertEqual([?TWO_PEER_2, ?TWO_PEER_1, ?ONE_PEER_1], ClosestContacts)].

should_search_a_key_within_each_bucket_and_refresh_its_content({KbucketPid, Peer}) ->
    meck:expect(peer, iterative_find_peers, fun always_successful_iterative_find_peers/2),

    kbucket:refresh(KbucketPid),

    timer:sleep(50),
    [?_assertEqual([?ZERO_PEER_1],  kbucket:get(KbucketPid, 0)),
     ?_assertEqual([?ONE_PEER_1],   kbucket:get(KbucketPid, 1)),
     ?_assertEqual([?TWO_PEER_1],   kbucket:get(KbucketPid, 2)),
     ?_assertEqual([?THIRD_PEER_1], kbucket:get(KbucketPid, 3))].

always_successful_iterative_find_peers(_, Key) ->
    [?FAKE_PEER(Key)].

put_contacts(KbucketPid, []) ->
    ok;
put_contacts(KbucketPid, [Contact | Contacts]) ->
    ok = kbucket:put(KbucketPid, Contact),
    put_contacts(KbucketPid, Contacts).
