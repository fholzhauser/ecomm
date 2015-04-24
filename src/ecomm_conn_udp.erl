%%%===================================================================
%%% This module is NOT to be used directly, but via:
%%%
%%% ecomm_conn_mgr:enable/1
%%% ecomm_conn_mgr:disable/1
%%%===================================================================
-module(ecomm_conn_udp).

-behaviour(gen_server).

%% Internal API
-export([ensure_options/1, start/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % callbacks

%% internal use only
-record(ecomm_conn_udp,
	{listen_socket :: gen_udp:socket(),
	 peer          :: {inet:host(), inet:port()},
	 conn_stat     :: function(),
	 codec         :: function(),
	 app_handler   :: function(),
	 state         :: term()}).

%%%===================================================================
%%% Internal API
%%%===================================================================

start({ConnStatFn, CodecFn, AppHandlerFn}, InitAppState)
  when is_function(ConnStatFn, 1), is_function(CodecFn, 1), is_function(AppHandlerFn, 1) ->
    Callbacks = {ConnStatFn, CodecFn, AppHandlerFn},
    gen_server:start(?MODULE, [Callbacks, InitAppState], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{ConnStatFn, CodecFn, AppHandlerFn}, InitAppState]) ->
    {ok, #ecomm_conn_udp{conn_stat = ConnStatFn, 
			 codec = CodecFn,
			 app_handler = AppHandlerFn,
			 state = InitAppState}}.

handle_info({udp, {LSock, Peer}, Packet}, #ecomm_conn_udp{listen_socket = undefined} = S) ->
    S1 = S#ecomm_conn_udp{listen_socket = LSock, peer = Peer},
    S2 = handle_data(Packet, S1),
    {stop, normal, S2}.

terminate(_Reason, _State) -> ok.
handle_call(_Request, _From, State) -> {reply, ignored, State}.
handle_cast(_, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% UDP transport handling
%%%===================================================================

ensure_options({open, Opts}) ->
    BadKeys = gb_sets:from_list([list, binary, active, fd, udp_module, mode,
				 add_membership, drop_membership,
				 multicast_if, multicast_loop, multicast_ttl]),
    OptsSet = gb_sets:from_list(proplists:get_keys(Opts)),
    case gb_sets:to_list(gb_sets:intersection(OptsSet, BadKeys)) of
	[] -> {ok, [binary, {active, true} | Opts]};
	_ -> {error, bad_options}
    end.

%%%%%%%%%% result of handle_data function is result of gen_server:handle_info function

handle_data(PacketIn, #ecomm_conn_udp{listen_socket = LSock, peer = Peer, state = AppState,
				      conn_stat = ConnStat, 
				      codec = Codec, 
				      app_handler = AppHandler} = S) ->
    case conn_start(ConnStat, {LSock, Peer}, AppState) of
	{ok, AppState1} ->
	    S1 = S#ecomm_conn_udp{state = AppState1},
	    case decode(Codec, PacketIn) of
		{ok, Request} ->
		    case control(AppHandler, Request, Peer, AppState1) of
			{reply, Reply, {TargetIP, TargetPort}, AppState2} ->
			    case encode(Codec, Reply) of
				{ok, PacketOut} ->
				    S2 = S1#ecomm_conn_udp{state = AppState2},
				    case send_data(LSock, {TargetIP, TargetPort}, PacketOut) of
					ok ->
					    {stop, Reason, AppState3} =
						conn_stop(ConnStat, {LSock, Peer}, AppState2),
					    {stop, Reason, S#ecomm_conn_udp{state = AppState3}};
					{stop, Reason} ->
					    {stop, Reason, S2}
				    end;
				Error ->
				    {stop, Error, S1}
			    end;
			{noreply, AppState2} ->
			    {stop, Reason, AppState3} = conn_stop(ConnStat, {LSock, Peer}, AppState2),
			    {stop, Reason, S#ecomm_conn_udp{state = AppState3}};
			{stop, Reason} ->
			    {stop, Reason, S1}
		    end;
		Error ->
		    {stop, Error, S}
	    end;
	{stop, Reason, AppState1} ->
	    {stop, Reason, S#ecomm_conn_udp{state = AppState1}}
    end.


%%%%%%%%%% no gen_server state here

conn_start(ConnStat, {LSock, Peer}, AppState) ->
    try ConnStat({start, udp, {LSock, Peer}, AppState}) of
	{ok, AppState1} ->
	    {ok, AppState1};
	{stop, Reason, AppState1} ->
	    {stop, Reason, AppState1};
	_Invalid ->
	    {stop, {error, udp_conn_start_return}, AppState}
    catch
	Ex:Err ->
	    {stop, {Ex, Err}, AppState}
    end.

decode(Codec, Packet) ->
    try Codec({decode, Packet}) of
	{ok, Term} ->
	    {ok, Term};
	incomplete ->
	    {error, udp_incomplete};
	Error when element(1, Error) == error ->
	    Error;
	_Invalid ->
	    {error, udp_decode_return}
    catch
	Ex:Err ->
	    {Ex, Err}
    end.

control(AppHandler, Request, Peer, AppState) ->
    try AppHandler({Request, AppState}) of
	ok ->
	    {noreply, AppState};
	{ok, AppState1} ->
	    {noreply, AppState1};
	{ok, Reply, AppState1} ->
	    {reply, Reply, Peer, AppState1};
	{ok, Reply, AppState1, OtherPeer} ->
	    {reply, Reply, OtherPeer, AppState1};
	stop ->
	    {stop, normal};
	{stop, Reason} ->
	    {stop, Reason};
	_Invalid ->
	    {stop, {error, udp_control_return}}
    catch
	Ex:Err ->
	    {Ex, Err}
    end.

encode(Codec, Term) ->
    try Codec({encode, Term}) of
	{ok, Packet} when is_binary(Packet) ->
	    {ok, Packet};
	Error when element(1, Error) == error ->
	    Error;
	_Invalid ->
	    {error, udp_encode_return}
    catch
	Ex:Err ->
	    {Ex, Err}
    end.

send_data(Socket, {TargetIP, TargetPort}, Packet) ->
    gen_udp:send(Socket, TargetIP, TargetPort, Packet).

conn_stop(ConnStat, {LSock, Peer}, AppState) ->
    try ConnStat({stop, udp, {LSock, Peer}, AppState}) of
	{ok, AppState1} ->
	    {stop, normal, AppState1};
	_Invalid ->
	    {stop, {error, udp_conn_stop_return}, AppState}
    catch
	Ex:Err ->
	    {stop, {Ex, Err}, AppState}
    end.
