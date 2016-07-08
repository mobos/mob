-module(mob_router).

-export([deploy/1]).

-include("service_supervisor/service.hrl").

deploy(ParsedService) ->
    case mob_dht:where_deployed(ParsedService#service.name) of
        {error, not_found} ->
            case mob_dht:find_available_node(ParsedService) of
                {ok, Node} ->
                    remote_mob:run(Node, ParsedService),
                    Node;
                {error, Error} -> Error
            end;
        {found, _Node} -> already_deployed
    end.

