%%%===================================================================
%%% This module is NOT to be used directly, but via:
%%%
%%% ecomm_conn_mgr:enable/1
%%% ecomm_conn_mgr:disable/1
%%%===================================================================

-module(ecomm_conn_tcp).

-behaviour(gen_server).

%% Internal API
-export([ensure_options/1, start/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % callbacks

%% internal use only
-record(ecomm_conn_tcp, 
	{listen_socket :: gen_tcp:socket(),
	 socket        :: gen_tcp:socket(),
	 buffer        :: binary(),
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

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{ConnStatFn, CodecFn, AppHandlerFn}, InitAppState]) ->
    {ok, #ecomm_conn_tcp{buffer = <<>>,
			 conn_stat = ConnStatFn, 
			 codec = CodecFn,
			 app_handler = AppHandlerFn,
			 state = InitAppState}}.

handle_info({tcp_opened, {LSock, CSock}}, #ecomm_conn_tcp{socket = undefined} = S) ->
    handle_open(S#ecomm_conn_tcp{listen_socket = LSock, socket = CSock});
handle_info({tcp, _CSock, Packet}, S) ->
    handle_data(Packet, S);
handle_info({tcp_closed, _CSock}, S) ->
    handle_close(S).

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, #ecomm_conn_tcp{socket = Socket}) ->
    catch gen_tcp:close(Socket).

handle_call(_Request, _From, State) -> {reply, ignored, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==============================================
%%% TCP transport handling
%%%==============================================

ensure_options({listen, Opts}) ->
    BadKeys = gb_sets:from_list([list, binary, fd, tcp_module, active, deliver, exit_on_close, mode]),
    OptsSet = gb_sets:from_list(proplists:get_keys(Opts)),
    case gb_sets:to_list(gb_sets:intersection(OptsSet, BadKeys)) of
	[] -> {ok, [{active, true}, binary | Opts]};
	_ -> {error, bad_options}
    end.

%%%%%%%%%% result of handle_* functions are results of gen_server:handle_info function

handle_open(#ecomm_conn_tcp{listen_socket = LSock, socket = CSock, 
			    conn_stat = ConnStat, state = AppState} = S) ->
    case conn_start(ConnStat, {LSock, CSock}, AppState) of
	{ok, AppState1} ->
	    {noreply, S#ecomm_conn_tcp{state = AppState1}};
	{stop, Reason, AppState1} ->
	    {stop, Reason, S#ecomm_conn_tcp{state = AppState1}}
    end.

handle_data(PacketIn, #ecomm_conn_tcp{socket = CSock, buffer = Buffer,
				      codec = Codec, app_handler = AppHandler, 
				      state = AppState} = S) ->
    case decode(Codec, PacketIn, Buffer) of
	{ok, Request, Buffer1} ->
	    S1 = S#ecomm_conn_tcp{buffer = Buffer1},
	    case control(AppHandler, Request, AppState) of
		{reply, Reply, AppState1} ->
		    case encode(Codec, Reply) of
			{ok, PacketOut} ->
			    S2 = S1#ecomm_conn_tcp{state = AppState1},
			    case send_data(CSock, PacketOut) of
				ok -> 
				    {noreply, S2};
				{stop, Reason} ->
				    {stop, Reason, S2}
			    end;
			Error ->
			    {stop, Error, S1}
		    end;
		{noreply, AppState1} ->
		    {noreply, S1#ecomm_conn_tcp{state = AppState1}};
		{stop, Reason} ->
		    {stop, Reason, S1}
	    end;
	{incomplete, Buffer1} ->
	    {noreply, S#ecomm_conn_tcp{buffer = Buffer1}};
	Error ->
	    {stop, Error, S}
    end.

handle_close(#ecomm_conn_tcp{listen_socket = LSock, socket = CSock,
			     conn_stat = ConnStat, state = AppState} = S) ->
    case conn_stop(ConnStat, {LSock, CSock}, AppState) of
	{ok, AppState1} ->
	    {stop, normal, S#ecomm_conn_tcp{state = AppState1}};
	{stop, Reason, AppState1} ->
	    {stop, Reason, S#ecomm_conn_tcp{state = AppState1}}
    end.


%%%%%%%%%% no gen_server state here

conn_start(ConnStat, {LSock, CSock}, AppState) ->
    try ConnStat({start, tcp, {LSock, CSock}, AppState}) of
	{ok, AppState1} ->
	    {ok, AppState1};
	{stop, Reason, AppState1} ->
	    {stop, Reason, AppState1};
	_Invalid ->
	    {stop, {error, tcp_conn_start_return}, AppState}
    catch
	Ex:Err ->
	    {stop, {Ex, Err}, AppState}
    end.

decode(Codec, Packet, PrevChunk) ->
    Packet1 = <<PrevChunk/binary, Packet/binary>>,
    try Codec({decode, Packet1}) of
	{ok, Term} ->
	    {ok, Term, <<>>};
	{ok, Term, NextChunk} ->
	    {ok, Term, NextChunk};
	incomplete ->
	    {incomplete, Packet1};
	Error when element(1, Error) == error ->
	    Error;
	_Invalid ->
	    {error, tcp_decode_return}
    catch
	Ex:Err ->
	    {Ex, Err}
    end.

control(AppHandler, Request, AppState) ->
    try AppHandler({Request, AppState}) of
	ok ->
	    {noreply, AppState};
	{ok, AppState1} ->
	    {noreply, AppState1};
	{ok, Reply, AppState1} ->
	    {reply, Reply, AppState1};
	stop ->
	    {stop, normal};
	{stop, Reason} ->
	    {stop, Reason};
	_Invalid ->
	    {stop, {error, tcp_control_return}}
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
	    {error, tcp_encode_return}
    catch
	Ex:Err ->
	    {Ex, Err}
    end.

send_data(Socket, Packet) ->
    case gen_tcp:send(Socket, Packet) of
	ok ->
	    ok;
	{error, closed} ->
	    {stop, normal};
	{error, Reason} ->
	    {stop, {error, Reason}}
    end.

conn_stop(ConnStat, {LSock, CSock}, AppState) ->
    try ConnStat({stop, tcp, {LSock, CSock}, AppState}) of
	{ok, AppState1} ->
	    {stop, normal, AppState1};
	_Invalid ->
	    {stop, {error, tcp_conn_stop_return}, AppState}
    catch
	Ex:Err ->
	    {stop, {Ex, Err}, AppState}
    end.


