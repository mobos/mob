-module('mob_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

start(_StartType, DefaultArgs) ->
    SpecifiedArgs = init:get_arguments(),
    Args = args_utils:merge_args(DefaultArgs, SpecifiedArgs),
    'mob_sup':start_link(Args).

stop(_State) ->
    ok.
