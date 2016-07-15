-module(remote_mob).

-export([connect/1]).
-export([peer/1]).

connect(NodeName) ->
    net_kernel:connect_node(NodeName).

peer(Node) ->
    gen_server:call({mob, Node}, peer).

