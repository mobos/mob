-module(service).
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

    #service{
       name = binary_to_list(BinaryName),
       command = binary_to_list(BinaryCommand)
    }.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/service.hrl").
-endif.
