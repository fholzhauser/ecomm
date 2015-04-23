-module(ecomm_conn_mgr).

-export([start/0, 
	 enable/1,
	 enable_tcp/4]). % API

-export([echo_callbacks/0,        %% testing - sends back everything
	 echo_dump_callbacks/1]). %% testing - also dumps to file

-define(DEFAULT_TCP_ACCEPTORS, 2).

-compile(export_all).

%%%==============================================
%%% Connection manager API
%%%==============================================

start() ->
    ecomm_conn_mgr_sup:start_link().

%% TCPOpts - options to gen_tcp:listen ++ [{num_acceptors, integer()}]
enable({tcp, ListenPort, TCPOpts, Callbacks}) ->
    enable({tcp, ListenPort, TCPOpts, Callbacks, undefined});
enable({tcp, ListenPort, TCPOpts, Callbacks, AppState}) ->
    enable_tcp(ListenPort, TCPOpts, Callbacks, AppState).

%% enable({udp, RecvPort, UDPOpts, Callbacks}) ->
%%     enable({udp, RecvPort, UDPOpts, Callbacks, undefined});
%% enable({udp, RecvPort, UDPOpts, Callbacks, AppState}) ->
%%     enable_udp(RecvPort, UDPOpts, Callbacks, AppState).
    

disable({tcp, ListenPort}) ->
    disable_tcp(ListenPort).


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

enable_tcp(ListenPort, TCPOpts, {ConnStatFn, CodecFn, AppHandlerFn} = Callbacks, AppState)
  when is_integer(ListenPort),
       is_list(TCPOpts),
       is_function(ConnStatFn, 1), is_function(CodecFn, 1), is_function(AppHandlerFn, 1) ->
    case ensure_tcp_options(TCPOpts) of
	{ok, TCPOpts1, NumAcceptors} ->
	    CSockFn = fun (Socket) -> start_tcp_handler(Socket, Callbacks, AppState) end,
	    ecomm_tcp_listeners_sup:add_listener_sup(ListenPort, TCPOpts1, CSockFn, NumAcceptors);
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

disable_tcp(ListenPort) when is_integer(ListenPort) ->
    case whereis(ecomm_tcp_listener_sup:name(ListenPort)) of
	undefined -> 
	    {error, not_found};
	ListenerSup -> 
	    supervisor:terminate_child(ecomm_tcp_listeners_sup, ListenerSup)
    end.


%%%%%%%%%%


%% enable_udp(RecvPort, UDPOpts, {ConnStatFn, CodecFn, AppHandlerFn} = Callbacks, AppState)
%%   when is_integer(RecvPort),
%%        is_list(UDPOpts),
%%        is_function(ConnStatFn, 1), is_function(CodecFn, 1), is_function(AppHandlerFn, 1) ->
%%     case ensure_udp_options(UDPOpts) of
%% 	{ok, UDPOpts1} ->
%% 	    CSockFn = fun (Socket) -> start_udp_handler(Socket, Callbacks, AppState) end,
%% 	    ecomm_udp_listeners_sup:add_listener_sup(RecvPort, UDPOpts1, CSockFn);
%% 	{error, Reason} ->
%% 	    {error, Reason}
%%     end.
    
    
%%%==============================================
%%% Testing callbacks
%%%==============================================

echo_callbacks() ->
    {fun ({start, _Socket, AppState}) -> {ok, AppState};
	 ({stop, _Socket, AppState}) -> {ok, AppState}
     end,
     fun ({decode, Packet}) -> {ok, Packet};
	 ({encode, Packet}) -> {ok, Packet}
     end,
     fun ({Line, AppState}) -> {ok, Line, AppState} end}.

echo_dump_callbacks(ToFilename) ->
    {fun ({start, _Socket, undefined}) -> {ok, ToFilename};
	 ({stop, _Socket, Filename}) -> {ok, Filename}
     end,
     fun ({decode, Packet}) -> {ok, Packet};
	 ({encode, Packet}) -> {ok, Packet}
     end,
     fun ({Line, Filename}) ->
	     ok = file:write_file(Filename, Line, [append]),
	     {ok, Line, Filename}
     end}.


%%%%%%%%%%

%% test() ->
%%     start(),
%%     Cs = echo_callbacks(),
%%     enable({tcp, 11111, [], Cs}).

%% dbgon(ecomm_conn_mgr), [dbgadd(M) || M <- [ecomm_conn_mgr_sup, ecomm_tcp_acceptors_sup, ecomm_tcp_listener_sup, ecomm_tcp_listeners_sup, ecomm_tcp_listener, ecomm_tcp_acceptor]].


