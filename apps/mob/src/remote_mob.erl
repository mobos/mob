-module(remote_mob).

-export([connect/1]).
-export([run/2]).
-export([peer/1]).

connect(NodeName) ->
    net_kernel:connect_node(NodeName).

run(Node, Service) ->
    gen_server:cast({mob, Node}, {run, Service}).

peer(Node) ->
    gen_server:call({mob, Node}, peer).
