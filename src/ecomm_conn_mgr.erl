-module(ecomm_conn_mgr).

-export([start/0, 
	 enable/1,
	 enable_tcp/4]). % API

%% testing callbacks
-export([echo_callbacks/0,        %% sends back everything
	 echo_dump_callbacks/1]). %% also dumps to file
%%

-define(DEFAULT_TCP_ACCEPTORS, 2).

-compile(export_all).

%%%==============================================
%%% Connection manager API
%%%==============================================

start() ->
    ecomm_conn_mgr_sup:start().

%% TCPOpts - options to gen_tcp:listen ++ [{num_acceptors, integer()}]
enable({tcp, ListenPort, TCPOpts, Callbacks}) ->
    enable({tcp, ListenPort, TCPOpts, Callbacks, undefined});
enable({tcp, ListenPort, TCPOpts, Callbacks, AppState}) ->
    enable_tcp(ListenPort, TCPOpts, Callbacks, AppState).

%% enable({udp, RecvPort, UDPOpts, {CodecFn, AppHandlerFn}}) ->
%%     gen_server:call(?MODULE, {enable, {udp, ListenPort, TCPOpts, {ConnStatFn, CodecFn, AppHandlerFn}}}).

enable_tcp(ListenPort, TCPOpts, {CodecFn, AppHandlerFn, ConnStatFn} = Callbacks, AppState)
  when is_integer(ListenPort),
       is_list(TCPOpts),
       is_function(CodecFn, 1), is_function(AppHandlerFn, 1), is_function(ConnStatFn, 1) ->
    case ensure_tcp_options(TCPOpts) of
	{ok, TCPOpts1, Accs} ->
	    CSockFn = fun (Socket) -> start_tcp_handler(Socket, Callbacks, AppState) end,
	    ecomm_tcp_listener_sup:add_listener(ListenPort, TCPOpts1, CSockFn, Accs);
	{error, Reason} ->
	    {error, Reason}
    end.

%%%%%%%%%%

ensure_tcp_options(TCPOpts) ->
    TCPOpts1 = ecomm_util_proplist:del(num_acceptors, TCPOpts),
    NumAcceptors = ecomm_util_proplist:getv(num_acceptors, TCPOpts, ?DEFAULT_TCP_ACCEPTORS),
    case ecomm_conn_tcp:ensure_options({listen, TCPOpts1}) of
	{ok, TCPOpts2} ->
	    {ok, TCPOpts2, NumAcceptors};
	{error, Reason} ->
	    {error, Reason}
    end.

start_tcp_handler(Socket, Callbacks, AppState) ->
    {ok, HandlerPid} = ecomm_conn_tcp:start_link(Callbacks, AppState),
    case gen_tcp:controlling_process(Socket, HandlerPid) of
	ok ->
	    erlang:send(HandlerPid, {tcp_opened, Socket});
	{error, closed} ->
	    ecomm_conn_tcp:stop(HandlerPid);
	{error, Reason} ->
	    ecomm_conn_tcp:stop(HandlerPid),
	    error({ecomm_tcp_ownership, Reason})
    end.



%%%==============================================
%%% Testing callbacks
%%%==============================================

echo_callbacks() ->
    {fun ({open, _Socket, AppState}) -> {ok, AppState};
	 ({close, _Socket, AppState}) -> {ok, AppState}
     end,
     fun ({decode, Packet}) -> {ok, Packet};
	 ({encode, Packet}) -> {ok, Packet}
     end,
     fun ({Line, AppState}) -> {ok, Line, AppState} end}.

echo_dump_callbacks(ToFilename) ->
    {fun ({open, _Socket, undefined}) ->
	     {ok, ToFilename};
	 ({close, _Socket, Filename}) ->
	     {ok, Filename}
     end,
     fun ({decode, Packet}) -> {ok, Packet};
	 ({encode, Packet}) -> {ok, Packet}
     end,
     fun ({Line, Filename}) ->
	     ok = file:write_file(Filename, Line, [append]),
	     {ok, Line, Filename}
     end}.

