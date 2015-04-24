-module(ecomm_udp_listener_sup).

-behaviour(supervisor).

-export([start_link/3, name/1]). % API

-export([init/1]). %% Supervisor callbacks

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, Opts, PacketFn) ->
    supervisor:start_link({local, name(Port)}, ?MODULE, [Port, Opts, PacketFn]).

name(Port) ->
    binary_to_atom(<<"ecomm_udp_listener_sup_", (integer_to_binary(Port))/binary>>, utf8).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Port, Opts, PacketFn]) ->
    Spec = [{ecomm_udp_listener,
	     {ecomm_udp_listener, start_link, [Port, Opts, PacketFn]}, permanent, 2000, worker,
	     [ecomm_udp_listener]}],
    {ok, {{one_for_one, 1, 1}, Spec}}.


