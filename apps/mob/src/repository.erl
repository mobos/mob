-module(repository).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
        {ok, #{}}.

handle_call({set, {Key, Value}}, _From, State) ->
        NewState = State#{Key => Value},
        Reply = ok,
        {reply, Reply, NewState};

handle_call({is_key, Key}, _From, State) ->
        Reply = maps:is_key(Key, State),
        {reply, Reply, State};

handle_call({get, Key}, _From, State) ->
        Reply = case maps:is_key(Key, State) of
            true ->
                #{Key := Value} = State,
                {found, Value};
            false ->
                {not_found, undefined}
        end,
        {reply, Reply, State}.

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
