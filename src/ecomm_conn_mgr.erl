-module(ecomm_conn_mgr).

-export([start/0, enable/1, disable/1]). % API

-export([echo_test/2,
	 echo_callbacks/0,        %% testing - sends back everything
	 echo_dump_callbacks/1]). %% testing - also dumps to file

-define(DEFAULT_TCP_ACCEPTORS, 2).

%%%==============================================
%%% Connection manager API
%%%==============================================

start() ->
    ecomm_conn_mgr_sup:start_link().

enable({Protocol, Port, Options, AppHandler}) 
  when is_function(AppHandler, 1) ->
    Callbacks = {fun default_conn_stat/1, fun default_codec/1, AppHandler},
    enable({Protocol, Port, Options, Callbacks});
enable({Protocol, Port, Options, {Codec, AppHandler}}) 
  when is_function(Codec, 1), is_function(AppHandler, 1) ->
    enable({Protocol, Port, Options, {fun default_conn_stat/1, Codec, AppHandler}});

%% TCPOpts - options to gen_tcp:listen ++ [{num_acceptors, integer()}]
enable({tcp, ListenPort, TCPOpts, Callbacks}) ->
    enable({tcp, ListenPort, TCPOpts, Callbacks, undefined});
enable({tcp, ListenPort, TCPOpts, Callbacks, AppState}) ->
    enable_tcp(ListenPort, TCPOpts, Callbacks, AppState);

enable({udp, RecvPort, UDPOpts, Callbacks}) ->
    enable({udp, RecvPort, UDPOpts, Callbacks, undefined});
enable({udp, RecvPort, UDPOpts, Callbacks, AppState}) ->
    enable_udp(RecvPort, UDPOpts, Callbacks, AppState).
    

disable({tcp, ListenPort}) ->
    disable_tcp(ListenPort);
disable({udp, RecvPort}) ->
    disable_udp(RecvPort).

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
	    CSockFn = fun (LSock, CSock) -> 
			      start_tcp_handler({LSock, CSock}, Callbacks, AppState)
		      end,
	    ecomm_tcp_listeners_sup:add_listener_sup(ListenPort, TCPOpts1, CSockFn, NumAcceptors);
	{error, Reason} ->
	    {error, Reason}
    end.

start_tcp_handler({LSock, CSock}, Callbacks, AppState) ->
    {ok, HandlerPid} = ecomm_conn_tcp:start(Callbacks, AppState),
    case gen_tcp:controlling_process(CSock, HandlerPid) of
	ok ->
	    erlang:send(HandlerPid, {tcp_opened, {LSock, CSock}});
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

ensure_udp_options(UDPOpts) ->
    ecomm_conn_udp:ensure_options({open, UDPOpts}).

enable_udp(RecvPort, UDPOpts, {ConnStatFn, CodecFn, AppHandlerFn} = Callbacks, AppState)
  when is_integer(RecvPort),
       is_list(UDPOpts),
       is_function(ConnStatFn, 1), is_function(CodecFn, 1), is_function(AppHandlerFn, 1) ->
    case ensure_udp_options(UDPOpts) of
	{ok, UDPOpts1} ->
	    PacketFn = fun ({LSock, Peer, Packet}) ->
			       start_udp_handler({LSock, Peer}, Packet, Callbacks, AppState)
		       end,
	    ecomm_udp_listeners_sup:add_listener_sup(RecvPort, UDPOpts1, PacketFn);
	{error, Reason} ->
	    {error, Reason}
    end.

start_udp_handler({LSock, Peer}, Packet, Callbacks, AppState) ->
    {ok, HandlerPid} = ecomm_conn_udp:start(Callbacks, AppState),
    erlang:send(HandlerPid, {udp, {LSock, Peer}, Packet}).

disable_udp(RecvPort) when is_integer(RecvPort) ->
    case whereis(ecomm_udp_listener_sup:name(RecvPort)) of
	undefined ->
	    {error, not_found};
	ListenerSup -> 
	    supervisor:terminate_child(ecomm_udp_listeners_sup, ListenerSup)
    end.


%%%==============================================
%%% Default pass-through callbacks
%%%==============================================

default_conn_stat({start, _Protocol, _ProtocolInfo, AppState}) -> {ok, AppState};
default_conn_stat({stop, _Protocol, _ProtocolInfo, AppState}) -> {ok, AppState}.

default_codec({decode, Packet}) -> {ok, Packet};
default_codec({encode, Packet}) -> {ok, Packet}.
    
%%%==============================================
%%% Testing callbacks
%%%==============================================

echo_callbacks() ->
    {fun ({start, _Protocol, _ProtocolInfo, AppState}) -> {ok, AppState};
	 ({stop, _Protocol, _ProtocolInfo, AppState}) -> {ok, AppState}
     end,
     fun ({decode, Packet}) -> {ok, Packet};
	 ({encode, Packet}) -> {ok, Packet}
     end,
     fun ({Line, AppState}) -> {ok, Line, AppState} end}.

echo_dump_callbacks(ToFilename) ->
    {fun ({start, _Protocol, _ProtocolInfo, undefined}) -> {ok, ToFilename};
	 ({stop, _Protocol, _ProtocolInfo, Filename}) -> {ok, Filename}
     end,
     fun ({decode, Packet}) -> {ok, Packet};
	 ({encode, Packet}) -> {ok, Packet}
     end,
     fun ({Line, Filename}) ->
	     ok = file:write_file(Filename, Line, [append]),
	     {ok, Line, Filename}
     end}.

%%%%%%%%%%

echo_test(Proto, Port) when Proto == tcp; Proto == udp ->
    enable({Proto, Port, [], echo_callbacks()}).
