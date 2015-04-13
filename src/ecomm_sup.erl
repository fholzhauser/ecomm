-module(ecomm_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(
        ecomm_config_store,
        [named_table, ordered_set, public, {read_concurrency, true}
    ]),
    %% TODO: This is a bit rude. The callback might crash the supervisor
    case application:get_env(ecomm, initial_config_callback) of
        {ok, {Module, Function}} ->
            Module:Function();
        _ ->
            ok
    end,
    {ok, {{one_for_one, 5, 10}, []}}.
