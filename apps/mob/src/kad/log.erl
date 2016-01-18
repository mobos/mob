-module(log).
-export([peer/3, peer/4]).
-export([kbucket/3, kbucket/4]).
-export([pid_to_field/2]).
-export([contact_to_field/2]).
-export([info/3, info/2]).
-export([notice/2]).

-include("peer.hrl").
-record(kbucket, {peer, k, contacts, keylength}).

peer(Peer, Fields, Message) ->
    peer(Peer, Fields, Message, []).
peer(Peer, Fields, Message, Args) ->
    #peer{kbucket = Kbucket, repository = Repository, mycontact = {_, Id}} = Peer,
    PeerFields = [{kbucket, pid_to_list(Kbucket)},
                  {repository, pid_to_list(Repository)},
                  {id, Id},
                  {module, peer}],
    info(PeerFields ++ Fields, Message, Args).

info(Message, Args) ->
    info([], Message, Args).
info(Fields, Message, Args) ->
    lager:info(Fields, Message, Args).

notice(Message, Args) ->
    lager:notice([], Message, Args).

kbucket(Kbucket, Fields, Message) ->
    kbucket(Kbucket, Fields, Message, []).
kbucket(Kbucket, Fields, Message, Args) ->
    #kbucket{peer = Peer, k = K, contacts = Contacts, keylength = Keylength} = Kbucket,
    KbucketFields = [contact_to_field(Peer, "peer"),
                     {k, K},
                     {keylength, Keylength},
                     {module, kbucket}],
    info(KbucketFields ++ Fields, Message, Args).

pid_to_field(Pid, Prefix) ->
    Field = list_to_atom(Prefix ++ "_pid"),
    [{Field, pid_to_list(Pid)}].

contact_to_field({PeerPid, PeerId}, Prefix) ->
    IdField = list_to_atom(Prefix ++ "_id"),
    [pid_to_field(PeerPid, Prefix), {IdField, PeerId}].
