%%%-------------------------------------------------------------------
%% @doc mob top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('mob_sup').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Args]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    ChildSpecs = [#{id => tcp_adapter,
                    start => {tcp_adapter, start_link, [Args]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [tcp_adapter]}],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
