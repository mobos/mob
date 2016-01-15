-module(service).

-behaviour(gen_fsm).

-export([spawn/1]).
-export([start/1]).
-export([parse/1]).

-export([init/1,
     spawned/2,
     handle_event/3,
     handle_sync_event/4,
     handle_info/3,
     terminate/3,
     code_change/4]).

-include("service.hrl").

parse(Service) ->
    BinaryService = list_to_binary(Service),
    case jsx:is_json(BinaryService) of
    false -> {error, format_error};
    true ->  {ok, json_to_service(BinaryService)}
    end.

json_to_service(BinaryService) ->
    ParsedService = jsx:decode(BinaryService),
    {<<"name">>, BinaryName} = lists:keyfind(<<"name">>, 1, ParsedService),
    {<<"command">>, BinaryCommand} = lists:keyfind(<<"command">>, 1, ParsedService),

    #service{
       name = binary_to_atom(BinaryName, utf8),
       command = binary_to_list(BinaryCommand)
    }.

spawn(Service = #service{name = ServiceName}) ->
    gen_fsm:start({local, ServiceName}, ?MODULE, [Service], []).

start(ServiceName) ->
    gen_fsm:send_event(ServiceName, start).

%% gen_fsm callbacks

init([Service]) ->
    log:log("[~p] Spawned Service: ~p", [?MODULE, Service#service.name]),
    {ok, spawned, Service}.

spawned(start, State) ->
    log:log("[~p] Starting Service: ~p", [?MODULE, State#service.name]),
    {next_state, spawned, State};
spawned(_Event, State) ->
    {next_state, spawned, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/service.hrl").
-endif.
