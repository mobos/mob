-module(docker).

-behaviour(gen_server).

-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-define(DOCKER_PATH, "/usr/bin/docker").

-record(state, {params, container_id, waiter, container_exit_code}).

init(Params) ->
    process_flag(trap_exit, true),
    {ok, #state{params = Params}}.

handle_call(start, _From, State) ->
    {Reply, NewState} = handle_start(State),
    {reply, Reply, NewState};
handle_call(stop, _From, State) ->
    {Reply, NewState} = handle_stop(State),
    {reply, Reply, NewState};
handle_call(pid, _From, State) ->
    {Reply, NewState} = handle_pid(State),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Handlers

handle_info({stdout, _Waiter, BinaryStdout}, State) ->
    ContainerExitCode = list_to_integer(clean_stdout_line(BinaryStdout)),
    {noreply, State#state{container_exit_code = ContainerExitCode}};

handle_info({'EXIT', _Waiter, _Reason}, State = #state{container_exit_code = undefined}) ->
    %% XXX If we catch an EXIT but the exit_code of the container is undefined
    %% means that the `docker wait` is exited (maybe for an error or because was killed)
    %% A simply rerun of `docker wait` can works but what if meanwhile the container
    %% is exited?
    {ok, NewWaiter} = run_waiter(State#state.container_id),
    {noreply, State#state{waiter = NewWaiter}};

handle_info({'EXIT', _Waiter, _Reason}, State = #state{container_exit_code = ExitCode}) ->
    {stop, {shutdown, {exit_code, ExitCode}}, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_start(State = #state{params = Params}) ->
    #{"image" := Image} = Params,
    #{"command" := Command} = Params,

    {ok, ContainerId} = run_container(Image, Command),
    {ok, Waiter} = run_waiter(ContainerId),

    {ok, State#state{waiter = Waiter, container_id = ContainerId}}.

handle_stop(State = #state{container_id = ContainerId}) ->
    {ok, {exit_code, 0}, _} = process:syn_exec([?DOCKER_PATH, "kill", ContainerId]),
    {ok, State#state{container_id = undefined}}.

handle_pid(State = #state{container_id = ContainerId}) ->
    {ContainerId, State}.

clean_stdout_line(BinaryStdout) ->
    string:strip(binary_to_list(BinaryStdout), right, $\n).

run_container(Image, Command) ->
    RunCommandLine = [?DOCKER_PATH, "run", "-d", Image, "sh", "-c", Command],
    {ok, {exit_code, 0}, [BinaryContainerId]} = process:syn_exec(RunCommandLine),
    ContainerId = clean_stdout_line(BinaryContainerId),
    {ok, ContainerId}.

run_waiter(ContainerId) ->
    process:exec([?DOCKER_PATH, "wait", ContainerId], [stdout]).
