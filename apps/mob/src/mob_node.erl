-module(mob_node).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([run/2]).
-export([name/0]).

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
    gen_server:start_link({local, mob_node}, mob_node, [], []).

name() ->
    node().

run(Node, Service) ->
    gen_server:cast({mob_node, Node}, {run, Service}).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({run, Service}, State) ->
    handle_run(Service),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_run(Service) ->
    %% XXX Currently the service is annunced *BEFORE* the service
    %% is really spawned. To be sure we should notify this module
    %% when the service is really spawned and proceed with the
    %% announce
    log:notice("[~p] Run request for ~p", [?MODULE, Service#service.name]),
    Services = mob_dht:services(),
    ServicesList = sets:to_list(Services),
    service_supervisor:run(Service, ServicesList),
    mob_dht:announce_spawned_service(Service, name()).
