-module(args_utils).

-export([merge_args/2]).
-export([get/2]).

merge_args(DefaultArgs, SpecifiedArgs) ->
    Unspecified = lists:foldl(fun({DefOpt, DefValue}, Args) ->
                    case lists:keyfind(DefOpt, 1, SpecifiedArgs) of
                        false -> [{DefOpt, DefValue} | Args];
                        _ -> Args
                    end
                end, [], DefaultArgs),
    lists:merge(Unspecified, SpecifiedArgs).

get(Option, Args) ->
    case lists:keyfind(Option, 1, Args) of
        {Option, Value} -> Value;
        _ -> []
    end.
