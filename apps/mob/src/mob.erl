-module(mob).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([join/1]).
-export([node_name/0]).
-export([peer/1]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {peer}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(NodeName) ->
    gen_server:call(?SERVER, {join, NodeName}).

peer(Node) ->
    gen_server:call({?SERVER, Node}, peer).

node_name() ->
    node().

%% Callbacks

init([]) ->
    Id = peer:hash_key(node()),
    Peer = peer:start(Id, 20, 3),
    {ok, #state{peer = Peer}}.

handle_call(peer, _From, State = #state{peer = Peer}) ->
    {reply, Peer, State};

handle_call({join, NodeName}, _From, State = #state{peer = Peer}) ->
    ConnectionResult = net_kernel:connect_node(NodeName),
    BootstrapPeer = peer(NodeName),
    peer:join(Peer, BootstrapPeer),
    {reply, ConnectionResult, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
