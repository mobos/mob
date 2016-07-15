-module(kbucket_t).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/test_macro.hrl").

-define(PEER_ID, 2#1101).
-define(ALPHA, 3).

-define(ID_BIT_LENGTH, 4).
-define(K, 3).

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
    meck:expect(peer, start, fun(Id, _, _) -> ?FAKE_PEER(Id) end),
    KbucketPid = kbucket:start(?K, ?ID_BIT_LENGTH),
    Peer = peer:start(?PEER_ID, KbucketPid, ?ALPHA),
    kbucket:set_peer(KbucketPid, Peer),
    {KbucketPid, Peer}.

teardown(_) ->
    ?assert(meck:validate(peer)),
    meck:unload(peer).

peer_suite_test_() ->
    [?setup(fun compute_the_distance_of_two_id/1),
     ?setup(fun returns_the_correct_bucket_index_of_a_distance/1),
     ?setup(fun start_a_kbucket_process/1),
     ?setup(fun create_a_bucket_if_not_exists/1),
     ?setup(fun returns_an_empty_list_for_an_unknown_bucket_index/1),
     ?setup(fun put_a_contact_with_the_same_bucket_index_to_the_tail/1),
     ?setup(fun move_to_the_tail_an_already_present_contact/1),
     ?setup(fun ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_reply/1),
     ?setup(fun ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_reply/1),
     ?setup(fun do_not_ping_the_least_seen_contact_if_it_is_the_same_to_put_but_update_it/1),
     ?setup(fun returns_the_k_contacts_closest_to_a_key/1),
     ?setup(fun fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full/1),
     ?setup(fun returns_less_than_k_contacts_if_not_all_available/1),
     ?setup(fun search_a_key_within_each_bucket_and_refresh_its_content/1)].

compute_the_distance_of_two_id(_) ->
    [?_assertEqual(3, kbucket:distance(2#0001, 2#0010))].

returns_the_correct_bucket_index_of_a_distance(_) ->
    [?_assertEqual(0, kbucket:bucket_index(1)),
     ?_assertEqual(1, kbucket:bucket_index(2)),
     ?_assertEqual(1, kbucket:bucket_index(3)),
     ?_assertEqual(2, kbucket:bucket_index(4)),
     ?_assertEqual(2, kbucket:bucket_index(5)),
     ?_assertEqual(2, kbucket:bucket_index(6)),
     ?_assertEqual(2, kbucket:bucket_index(7)),
     ?_assertEqual(3, kbucket:bucket_index(8)),
     ?_assertEqual(7, kbucket:bucket_index(130))].

start_a_kbucket_process({KbucketPid, _}) ->
    [?_assert(erlang:is_pid(KbucketPid))].

create_a_bucket_if_not_exists({KbucketPid, _}) ->
    ok = kbucket:put(KbucketPid, ?ONE_PEER_1),

    [?_assertEqual([?ONE_PEER_1], kbucket:get(KbucketPid, 1))].

returns_an_empty_list_for_an_unknown_bucket_index({KbucketPid, _}) ->
    [?_assertEqual([], kbucket:get(KbucketPid, 100))].

put_a_contact_with_the_same_bucket_index_to_the_tail({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?ONE_PEER_1, ?ONE_PEER_2]),

    [?_assertEqual([?ONE_PEER_1, ?ONE_PEER_2], kbucket:get(KbucketPid, 1))].

move_to_the_tail_an_already_present_contact({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?ONE_PEER_1, ?ONE_PEER_2]),

    ok = kbucket:put(KbucketPid, ?ONE_PEER_1),

    [?_assertEqual([?ONE_PEER_2, ?ONE_PEER_1], kbucket:get(KbucketPid, 1))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_reply({KbucketPid, _}) ->
    meck:expect(peer, check_link,  ?two_any_args(?return(ko))),
    put_contacts(KbucketPid, [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3]),

    ok = kbucket:put(KbucketPid, ?TWO_PEER_4),

    ExpectedBucket = [?TWO_PEER_2, ?TWO_PEER_3, ?TWO_PEER_4],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_reply({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3]),
    meck:expect(peer, check_link, ?two_any_args(?return(ok))),

    ok = kbucket:put(KbucketPid, ?TWO_PEER_4),

    ExpectedBucket = [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2))].

do_not_ping_the_least_seen_contact_if_it_is_the_same_to_put_but_update_it({KbucketPid, Peer}) ->
    put_contacts(KbucketPid, [?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3]),

    ok = kbucket:put(KbucketPid, ?TWO_PEER_1),

    ExpectedBucket = [?TWO_PEER_2, ?TWO_PEER_3, ?TWO_PEER_1],
    [?_assertEqual(ExpectedBucket, kbucket:get(KbucketPid, 2)),
     ?_assertEqual(0, meck:num_calls(peer, check_link, [?TWO_PEER_1, Peer]))].

returns_the_k_contacts_closest_to_a_key({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?THIRD_PEER_1, ?TWO_PEER_3, ?TWO_PEER_2, ?TWO_PEER_1]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1101),

    [?_assertEqual([?TWO_PEER_1, ?TWO_PEER_2, ?TWO_PEER_3], ClosestContacts)].

fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?TWO_PEER_2, ?TWO_PEER_1, ?ONE_PEER_1]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1010),

    [?_assertEqual([?TWO_PEER_2, ?TWO_PEER_1, ?ONE_PEER_1], ClosestContacts)].

returns_less_than_k_contacts_if_not_all_available({KbucketPid, _}) ->
    put_contacts(KbucketPid, [?TWO_PEER_1, ?ONE_PEER_1]),

    ClosestContacts = kbucket:closest_contacts(KbucketPid, 2#1010),

    [?_assertEqual([?TWO_PEER_1, ?ONE_PEER_1], ClosestContacts)].

search_a_key_within_each_bucket_and_refresh_its_content({KbucketPid, _}) ->
    meck:expect(peer, iterative_find_peers, fun always_successful_iterative_find_peers/2),

    kbucket:refresh(KbucketPid),

    timer:sleep(50),
    [?_assertEqual([?ZERO_PEER_1],  kbucket:get(KbucketPid, 0)),
     ?_assertEqual([?ONE_PEER_1],   kbucket:get(KbucketPid, 1)),
     ?_assertEqual([?TWO_PEER_1],   kbucket:get(KbucketPid, 2)),
     ?_assertEqual([?THIRD_PEER_1], kbucket:get(KbucketPid, 3))].

always_successful_iterative_find_peers(_, Key) ->
    [?FAKE_PEER(Key)].

put_contacts(_KbucketPid, []) ->
    ok;
put_contacts(KbucketPid, [Contact | Contacts]) ->
    ok = kbucket:put(KbucketPid, Contact),
    put_contacts(KbucketPid, Contacts).
