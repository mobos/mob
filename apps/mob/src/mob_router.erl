-module(mob_router).

-export([deploy/1]).
-export([restart/1]).
-export([add_child/2]).
-export([is_started/1]).

-include("service_supervisor/service.hrl").

deploy(ParsedService) ->
    case mob_dht:where_deployed(ParsedService#service.name) of
        {error, not_found} ->
            case mob_dht:find_available_node(ParsedService) of
                {ok, Node} ->
                    mob_node:run(Node, ParsedService),
                    Node;
                {error, Error} -> Error
            end;
        {found, _Node} -> already_deployed
    end.

restart(ServiceName) ->
    case mob_dht:where_deployed(ServiceName) of
        {found, Node} -> mob_node:restart(Node, ServiceName);
        _             -> not_found
    end.

add_child(ParentName, ChildName) ->
    case mob_dht:where_deployed(ParentName) of
        {found, Node} -> mob_node:add_child(Node, ParentName, ChildName);
        _ -> not_found
    end.

is_started(ServiceName) ->
    case mob_dht:where_deployed(ServiceName) of
        {found, Node} -> mob_node:is_started(Node, ServiceName);
        _ -> false
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/mob_router.hrl").
-endif.
