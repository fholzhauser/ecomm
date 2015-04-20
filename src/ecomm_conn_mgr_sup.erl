-module(ecomm_conn_mgr_sup).
-behaviour(supervisor).

-export([start/0, init/1]).

%%%===================================================================
%%% Supervisor
%%%===================================================================

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 1}, [child_spec(C) || C <- [listener_sup]]}}.

%%%%%%%%%%

child_spec(listener_sup) ->
    {ecomm_tcp_listener_sup,
     {ecomm_tcp_listener_sup, start_link, []},
     permanent, 2000, supervisor, [ecomm_tcp_listener_sup]}.
