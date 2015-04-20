-module(ecomm_tcp_listener_sup).

-behaviour(supervisor).

-export([start_link/0, add_listener/4]). % API

-export([init/1]). %% Supervisor callbacks

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_listener(Port, Opts, CSockFn, NumAcceptors) ->
    supervisor:start_child(?MODULE, [Port, Opts, CSockFn, NumAcceptors]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Listener = ecomm_tcp_listener,
    ListenerSpec = {Listener, {Listener, start_link, []}, transient, 2000, worker, [Listener]},
    {ok, {{simple_one_for_one, 1, 1}, [ListenerSpec]}}.
