-module(remote_mob).
-export([run/2]).

run(Node, Service) ->
    gen_server:cast({mob, Node}, {run, Service}).
