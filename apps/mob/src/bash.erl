-module(bash).

-behaviour(gen_server).

-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {params, process}).

init(Params) ->
    {ok, #state{params = Params}}.

handle_call(start, _From, State = #state{params = Params}) ->
    #{"command" := Command} = Params,
    {ok, Process} = process:exec(["/bin/bash", "-c", Command]),
    {reply, ok, State#state{process = Process}};
handle_call(pid, _From, State = #state{process = Process}) ->
    Reply = process:os_pid(Process),
    {reply, Reply, State};
handle_call(stop, _From, State = #state{process = Process}) ->
    Reply = process:stop(Process),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', FromProcess, Reason}, #state{process = Process})
      when FromProcess =:= Process ->
    {stop, {shutdown, Reason}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).
-include_lib("../test/bash.hrl").
-endif.
