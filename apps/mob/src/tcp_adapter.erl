-module(tcp_adapter).

-export([start_link/1]).
-export([init_server/1]).

-record(tcp_options, {port}).

start_link(Args) ->
    TcpAdapterPid = spawn_link(?MODULE, init_server, [Args]),
    register(tcp_adapter, TcpAdapterPid),
    {ok, TcpAdapterPid}.

init_server(Args) ->
    Options = parse_options(Args),
    {ok, ListenSock} = gen_tcp:listen(Options#tcp_options.port, [binary, {packet, 0},
                                                                         {active, false}]),

    log:notice("[~s] Listen to port ~p", [?MODULE, Options#tcp_options.port]),
    spawn(fun() -> acceptor(ListenSock) end),
    timer:sleep(infinity).

acceptor(ListenSock) ->
    {ok, Socket} = gen_tcp:accept(ListenSock),
    spawn(fun() -> acceptor(ListenSock) end),
    listener(Socket).

listener(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            log:notice("[~p] Received: ~p", [?MODULE, Msg]),
            case command:exec_command_line(binary_to_list(Msg)) of
                {error, E} ->
                    log:notice("[~p] Error during exec: ~p ", [?MODULE, E]);
                {ok, Response} ->
                    log:notice("[~p] Sending Response: ~p", [?MODULE, Response]),
                    gen_tcp:send(Socket, Response)
            end
    end.

parse_options(Args) ->
    [Port | []] = args_utils:get(port, Args),
    #tcp_options{port = list_to_integer(Port)}.
