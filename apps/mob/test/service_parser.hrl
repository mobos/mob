-include_lib("eunit/include/eunit.hrl").

-define(SERVICE_NAME, my_service).
-define(SERVICE_COMMAND, "my command").
-define(SERVICE_REQUIRES, [first_dependency, second_dependency]).

-define(SIMPLE_SERVICE, #service{name = ?SERVICE_NAME, command = ?SERVICE_COMMAND}).

should_parse_a_service_test() ->
    Service = "{
                 \"name\": \""++ atom_to_list(?SERVICE_NAME) ++"\",
                 \"command\": \""++ ?SERVICE_COMMAND ++"\"
               }",

    {ok, ParsedService} = service_parser:parse(Service),

    ?assertEqual(?SERVICE_NAME, ParsedService#service.name),
    ?assertEqual(?SERVICE_COMMAND, ParsedService#service.command),
    ?assertEqual([], ParsedService#service.requires).

should_parse_a_service_with_dependencies_test() ->
    Service = "{
                \"name\": \"" ++ atom_to_list(?SERVICE_NAME) ++ "\",
                \"command\": \"" ++ ?SERVICE_COMMAND ++ "\",
                \"requires\": [\"first_dependency\", \"second_dependency\"]
               }",
    {ok, ParsedService} = service_parser:parse(Service),

    ?assertEqual(?SERVICE_NAME, ParsedService#service.name),
    ?assertEqual(?SERVICE_COMMAND, ParsedService#service.command),
    ?assertEqual(?SERVICE_REQUIRES, ParsedService#service.requires).

should_fail_if_the_format_isnt_json_test() ->
    Service = "{ not_valid_json",

    {error, Error} = service_parser:parse(Service),

    ?assertEqual(Error, format_error).

