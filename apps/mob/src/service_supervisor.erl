-module(service_supervisor).

-behaviour(gen_server).

-export([start_link/0]).
-export([run/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("service.hrl").

-define(SERVER, ?MODULE).

-record(state, {spawned}).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run(Service) ->
    log:log("[~p] Run Service: ~p", [?MODULE, Service#service.name]),
    gen_server:cast(?SERVER, {run, Service}).

%% Callbacks

init([]) ->
        {ok, #state{spawned = []}}.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({run, Service}, State) ->
    NewState = handle_run(Service, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%% Handlers

handle_run(Service, State = #state{spawned = Spawned}) ->
    ServiceName = Service#service.name,
    case is_spawned(ServiceName, State) of
        true ->
            service:start(ServiceName),
            State;
        false ->
            service:spawn(Service),
            service:start(ServiceName),
            State#state{spawned = [ServiceName | Spawned]}
    end.

is_spawned(ServiceName, #state{spawned = Spawned}) ->
    lists:member(ServiceName, Spawned).

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/service_supervisor.hrl").
-endif.
