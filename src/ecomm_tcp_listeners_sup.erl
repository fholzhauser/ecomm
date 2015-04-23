-module(ecomm_tcp_listeners_sup).

-behaviour(supervisor).

-export([start_link/0, add_listener_sup/4, del_listener_sup/1]). %% API

-export([init/1]). %% Supervisor callbacks

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_listener_sup(Port, Opts, CSockFn, NumAcceptors) ->
    supervisor:start_child(?MODULE, [Port, Opts, CSockFn, NumAcceptors]).

del_listener_sup(SupPid) ->
    supervisor:terminate_child(?MODULE, SupPid).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Spec = [{ecomm_tcp_listener_sup,
	     {ecomm_tcp_listener_sup, start_link, []}, permanent, 2000, supervisor, 
	     [ecomm_tcp_listener_sup]}],
    {ok, {{simple_one_for_one, 1, 1}, Spec}}.
