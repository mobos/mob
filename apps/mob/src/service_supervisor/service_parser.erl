-module(service_parser).

-export([parse/1]).

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

    BinaryRequires = get_or_default(<<"requires">>, [], ParsedService),
    BinaryRestart = get_or_default(<<"restart">>, <<"none">>, ParsedService),

    #service {
       name = binary_to_atom(BinaryName, utf8),
       command = binary_to_list(BinaryCommand),
       requires = [binary_to_atom(Dependency, utf8) || Dependency <- BinaryRequires],
       restart = binary_to_atom(BinaryRestart, utf8)
    }.

get_or_default(Key, Default, ParsedService) ->
    case lists:keyfind(Key, 1, ParsedService) of
        {Key, Value} -> Value;
        _ -> Default
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../../test/service_parser.hrl").
-endif.
