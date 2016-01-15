-module(remote_mob).

-export([run/2]).
-export([peer/1]).

run(Node, Service) ->
    gen_server:cast({mob, Node}, {run, Service}).

peer(Node) ->
    gen_server:call({mob, Node}, peer).
