-module(parser).

-export([parse_command_line/1]).

-define(OPTIONS, [
                  {node, 'n', "node", boolean, undefined},
                  {join, 'j', "join", {'string', undefined}, undefined}
                 ]).

parse_command_line(CommandLine) ->
    case getopt:parse(?OPTIONS, CommandLine) of
        {error, E} ->
            {error, E};
        {ok, {ParsedArguments, _Ignored}} ->
            {ok, ParsedArguments}
    end.
