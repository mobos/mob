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
    {<<"provider">>, BinaryProvider} = lists:keyfind(<<"provider">>, 1, ParsedService),
    {<<"params">>, BinaryParams} = lists:keyfind(<<"params">>, 1, ParsedService),

    BinaryRequires = get_or_default(<<"requires">>, [], ParsedService),
    BinaryRestart = get_or_default(<<"restart">>, <<"none">>, ParsedService),

    #service {
       name = binary_to_atom(BinaryName, utf8),
       provider = binary_to_atom(BinaryProvider, utf8),
       params = maps:from_list(binary_keylist_to_list(BinaryParams)),
       requires = [binary_to_atom(Dependency, utf8) || Dependency <- BinaryRequires],
       restart = binary_to_atom(BinaryRestart, utf8)
    }.

get_or_default(Key, Default, ParsedService) ->
    case lists:keyfind(Key, 1, ParsedService) of
        {Key, Value} -> Value;
        _ -> Default
    end.

binary_keylist_to_list(List) ->
    [{binary_to_list(BinaryKey), binary_to_list(BinaryValue)} || {BinaryKey, BinaryValue} <- List].

-ifdef(TEST).
-compile([export_all]).
-endif.
