-module(ecomm_tcp_listener_sup).

-behaviour(supervisor).

-export([start_link/4, name/1]). % API

-export([init/1]). %% Supervisor callbacks

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, Opts, CSockFn, NumAcceptors) ->
    supervisor:start_link({local, name(Port)}, ?MODULE, [Port, Opts, CSockFn, NumAcceptors]).

name(Port) ->
    binary_to_atom(<<"ecomm_tcp_listener_sup_", (integer_to_binary(Port))/binary>>, utf8).    
    
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Port, Opts, CSockFn, NumAcceptors]) ->
    Specs = [{ecomm_tcp_listener,
	      {ecomm_tcp_listener, start_link, [Port, Opts, CSockFn, NumAcceptors]},
	      permanent, 2000, worker, [ecomm_tcp_listener]},
	     {ecomm_tcp_acceptors_sup,
	      {ecomm_tcp_acceptors_sup, start_link, [Port]},
	      permanent, 2000, supervisor, [ecomm_tcp_acceptors_sup]}],
    {ok, {{rest_for_one, 1, 1}, Specs}}.
	   


