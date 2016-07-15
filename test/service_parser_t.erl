-module(service_parser_t).

-include_lib("eunit/include/eunit.hrl").

-include("apps/mob/src/service_supervisor/service.hrl").

-define(SERVICE_NAME, my_service).
-define(SERVICE_PROVIDER, bash).
-define(SERVICE_PARAMS, #{"command" => "a command"}).
-define(SERVICE_REQUIRES, [first_dependency, second_dependency]).

-define(SIMPLE_SERVICE, #service{name = ?SERVICE_NAME, command = ?SERVICE_COMMAND}).

should_parse_a_service_test() ->
    Service = "{
                 \"name\": \""++ atom_to_list(?SERVICE_NAME) ++"\",
                 \"provider\": \""++ atom_to_list(?SERVICE_PROVIDER) ++"\",
                 \"params\": {
                    \"command\": \"a command\"
                    }
               }",

    {ok, ParsedService} = service_parser:parse(Service),

    ?assertEqual(?SERVICE_NAME, ParsedService#service.name),
    ?assertEqual(?SERVICE_PROVIDER, ParsedService#service.provider),
    ?assertEqual(?SERVICE_PARAMS, ParsedService#service.params),
    ?assertEqual([], ParsedService#service.requires).

should_parse_a_service_with_dependencies_test() ->
    Service = "{
                 \"name\": \""++ atom_to_list(?SERVICE_NAME) ++"\",
                 \"provider\": \""++ atom_to_list(?SERVICE_PROVIDER) ++"\",
                 \"params\": {
                    \"command\": \"a command\"
                    },
                  \"requires\": [\"first_dependency\", \"second_dependency\"]
               }",

    {ok, ParsedService} = service_parser:parse(Service),

    ?assertEqual(?SERVICE_NAME, ParsedService#service.name),
    ?assertEqual(?SERVICE_PROVIDER, ParsedService#service.provider),
    ?assertEqual(?SERVICE_PARAMS, ParsedService#service.params),
    ?assertEqual(?SERVICE_REQUIRES, ParsedService#service.requires).

should_fail_if_the_format_isnt_json_test() ->
    Service = "{ not_valid_json",

    {error, Error} = service_parser:parse(Service),

    ?assertEqual(Error, format_error).

