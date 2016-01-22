-module(provider).

-export([init/2]).
-export([start/1]).
-export([stop/1]).
-export([pid/1]).

init(ProviderName, Params) ->
    gen_server:start_link(ProviderName, Params, []).

start(Provider) ->
    gen_server:call(Provider, start, infinity).

stop(Provider) ->
    gen_server:call(Provider, stop).

pid(Provider) ->
    gen_server:call(Provider, pid).
