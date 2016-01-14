-module(command).

-export([exec_command_line/1]).

exec_command_line(CommandLine) ->
    case parser:parse_command_line(CommandLine) of
        {ok, ParsedCommandLine} ->
            case execute_command(ParsedCommandLine) of
                unknown -> {error, "Unknow command"};
                {ok, Response} -> {ok, Response}
            end;
        {error, ParserError} -> {error, ParserError}
    end.

handle_command({node, true}, _CommandLine) ->
    {ok, atom_to_list(mob:node_name())};
handle_command({deploy, Service}, _CommandLine) ->
    Result = mob:deploy(Service),
    {ok, atom_to_list(Result)};
handle_command({join, JoinNode}, _CommandLine) ->
    NodeName = list_to_atom(JoinNode),
    Result = mob:join(NodeName),
    {ok, atom_to_list(Result)};
handle_command(_Msg, _) ->
    unknown.

execute_command(ParsedCommandLine) ->
    execute_command(ParsedCommandLine, ParsedCommandLine).
execute_command([], _ParsedCommandLine) ->
    unknown;
execute_command([Command | Commands], CommandsList) ->
    case handle_command(Command, CommandsList) of
        unknown -> execute_command(Commands, CommandsList);
        {ok, Response} -> {ok, Response}
    end.
