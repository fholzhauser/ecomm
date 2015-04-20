-module(ecomm_tcp_listener).

-behaviour(gen_server).

-export([start_link/3, start_link/4]). % API

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % callbacks

%%%===================================================================
%%% API
%%%===================================================================

%% CSockFn = fun (ClientSocket) -> .. gen_tcp:controlling_process(ClientSocket, NewOwnerPid) .. end
start_link(Port, Opts, CSockFn) ->
    start_link(Port, Opts, CSockFn, 1).

start_link(Port, Opts, CSockFn, NumAcceptors) 
  when is_integer(Port), 
       is_list(Opts), 
       is_function(CSockFn, 1),
       is_integer(NumAcceptors), NumAcceptors > 0 ->
    Name = binary_to_atom(<<"ecomm_tcp_listener_", (integer_to_binary(Port))/binary>>, utf8),
    gen_server:start_link({local, Name}, ?MODULE, [Port, Opts, CSockFn, NumAcceptors], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port, Opts, CSockFn, 1]) ->
    do_init(Port, Opts, CSockFn, 1);
init([Port, Opts, CSockFn, NumAcceptors]) ->
    Opts1 = [{reuseaddr, true} | ecomm_util_proplist:del(reuseaddr, Opts)],
    do_init(Port, Opts1, CSockFn, NumAcceptors).
    
do_init(Port, Opts, CSockFn, NumAcceptors) ->
    {ok, LSock} = gen_tcp:listen(Port, Opts),
    Acceptor = fun () -> acceptor_loop(LSock, CSockFn) end,
    {AccPids, _} = lists:unzip([spawn_monitor(Acceptor) || _ <- lists:seq(1, NumAcceptors)]),
    {ok, {LSock, CSockFn, AccPids}}.

terminate(_Reason, {LSock, _CSockFn}) ->
    catch gen_tcp:close(LSock).

handle_info({'DOWN', _, process, DeadAccPid, _Info}, {LSock, CSockFn, AccPids}) -> 
    {NewAccPid, _} = spawn_monitor(fun () -> acceptor_loop(LSock, CSockFn) end),
    {noreply, {LSock, CSockFn, [NewAccPid | AccPids -- [DeadAccPid]]}};

handle_info(_Msg, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Request, _From, State) -> {reply, ignored, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%

acceptor_loop(LSock, CSockFn) ->
    case gen_tcp:accept(LSock) of
	{ok, Sock} ->
	    CSockFn(Sock), %% CSockFn MUST disown client socket here (with gen_tcp:controlling_process)
	    acceptor_loop(LSock, CSockFn);
	{error, Reason} ->
	    error({ecomm_tcp_acceptor, Reason})
    end.
