-module(log).
-export([peer/3, peer/4]).
-export([kbucket/3, kbucket/4]).
-export([pid_to_field/2]).
-export([contact_to_field/2]).
-export([log/3, log/2]).

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
    log(PeerFields ++ Fields, Message, Args).

log(Message, Args) ->
    log([], Message, Args).
log(Fields, Message, Args) ->
    lager:info(Fields, Message, Args).

kbucket(Kbucket, Fields, Message) ->
    kbucket(Kbucket, Fields, Message, []).
kbucket(Kbucket, Fields, Message, Args) ->
    #kbucket{peer = Peer, k = K, contacts = Contacts, keylength = Keylength} = Kbucket,
    KbucketFields = [contact_to_field(Peer, "peer"),
                     {k, K},
                     {keylength, Keylength},
                     {module, kbucket}],
    log(KbucketFields ++ Fields, Message, Args).

pid_to_field(Pid, Prefix) ->
    Field = list_to_atom(Prefix ++ "_pid"),
    [{Field, pid_to_list(Pid)}].

contact_to_field({PeerPid, PeerId}, Prefix) ->
    IdField = list_to_atom(Prefix ++ "_id"),
    [pid_to_field(PeerPid, Prefix), {IdField, PeerId}].
