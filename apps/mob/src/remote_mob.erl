-module(remote_mob).

-export([connect/1]).
-export([run/2]).
-export([peer/1]).
-export([restart/2]).
-export([is_started/2]).
-export([add_child/3]).

connect(NodeName) ->
    net_kernel:connect_node(NodeName).

run(Node, Service) ->
    gen_server:cast({mob, Node}, {run, Service}).

peer(Node) ->
    gen_server:call({mob, Node}, peer).

is_started(Node, Service) ->
    gen_server:call({mob, Node}, {is_started, Service}).

restart(Node, Service) ->
    gen_server:cast({mob, Node}, {restart, Service}).

add_child(Node, Parent, Child) ->
    gen_server:cast({mob, Node}, {add_child, Parent, Child}).
