%% Basic temporary config store

%% It doesn't have persistence since most applications already have some sort
%% of configuration persistence ideas. Initial configuration callback is
%% configured in the ecomm application environment:
%%      initial_config_callback, {Module, Function}
%% Module:Function() gets the initial config from the persistent (local/remote)
%% config store and poplulates the local config.

%% Envisioned future features for configuration handling
%% - validation callbacks
%% - config change callbacks
%% - config info (description, validation rules)
-module(ecomm_config).
-export([delete/1, write/2, read/1, read/2, list/0]).

write(Name, Value) ->
    ets:insert(ecomm_config_store, {Name, Value}).

read(Name) ->
    read(Name, undefined).
read(Name, Default) ->
    case ets:lookup(ecomm_config_store, Name) of
        [] -> Default;
        [{Name, Value} | _] -> Value
    end.

delete(Name) ->
    ets:delete(ecomm_config_store, Name).

list() ->
    ets:tab2list(ecomm_config_store).
