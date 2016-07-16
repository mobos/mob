-module(mob).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([join/1]).
-export([deploy/1]).
-export([node_name/0]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-include("service_supervisor/service.hrl").

-record(state, {}).

start_link() ->
    gen_server:start_link({local, mob}, ?MODULE, [], []).

deploy(Service) ->
    gen_server:call(mob, {deploy, Service}).

join(NodeName) ->
    gen_server:call(mob, {join, NodeName}).

node_name() ->
    node().

%% Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({join, NodeName}, _From, State) ->
    ConnectionResult = mob_dht:join(NodeName),
    {reply, ConnectionResult, State};

handle_call({deploy, Service}, _From, State) ->
    Reply = handle_deploy(Service),
    {reply, Reply, State};

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

% Handlers

handle_deploy(Service) ->
    case service_parser:parse(Service) of
        {ok, ParsedService} -> mob_router:deploy(ParsedService);
        {error, Error} -> Error
    end.

-ifdef(TEST).
-compile([export_all]).
-endif.
