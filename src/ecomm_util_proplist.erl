-module(ecomm_util_proplist).
-export([
    getv/2, getv/3, setv/3, del/2, condv/4, mgetv/2, agetv/2, agetv/3, update/2
]).

%%%==============================================
%%% Helpers for nested proplists
%%%==============================================

% dive in to nested proplist
getv(Key, PL) -> getv(Key, PL, undefined).
getv(Key, PL, Default) when not is_list(Key) -> getv([Key], PL, Default);
getv([Key], PL, Default) -> proplists:get_value(Key, PL, Default);
getv([Key | Rest], PL, Default) ->
    case lists:keyfind(Key, 1, PL) of
        false -> Default;
        {Key, Val} when is_list(Val) ->
            case io_lib:char_list(Val) of
                true -> Default;
                _ -> getv(Rest, Val, Default)
            end;
        _ -> Default
    end.

% update nested proplist
setv(Key, Val, PL) when not is_list(Key) -> setv([Key], Val, PL);
setv([Key], Val, PL) -> lists:keystore(Key, 1, PL, {Key, Val});
setv([Key | Rest], Val, PL) ->
    CurrentProp = case lists:keyfind(Key, 1, PL) of
        {Key, Prop} when is_list(Prop) ->
            case io_lib:char_list(Prop) of true -> []; _ -> Prop end;
        _ -> []
    end,
    NKey = case io_lib:char_list(Key) of true -> [Key]; _ -> Key end,
    setv(NKey, setv(Rest, Val, CurrentProp), PL).

% delete from nested proplist
del(Key, PL) when not is_list(Key) -> del([Key], PL);
del([Key], PL) -> lists:keydelete(Key, 1, PL);
del([Key | Rest], PL) ->
    case proplists:get_value(Key, PL) of
        Prop when is_list(Prop) ->
            NKey = case io_lib:char_list(Key) of true -> [Key]; _ -> Key end,
            setv(NKey, del(Rest, Prop), PL);
        _ -> PL
    end.

% Generate a value conditionally on a proplist element, undefined is treated as
% normal value. If the value is found in the ValueMap which is a {Value, Mapped}
% mapping list then the Mapped value is returned unless Mapped is a fun in which
% case the result is fun(Value). If the value is not found in the mapping list
% then Default is returned. Again if Default is a fun then fun(Value) is returned
condv(Name, Proplist, ValueMap, Default) ->
    Value = getv(Name, Proplist),
    case lists:keyfind(Value, 1, ValueMap) of
        false when is_function(Default) -> Default(Value);
        false -> Default;
        {Value, Mapped} when is_function(Mapped)-> Mapped(Value);
        {Value, Mapped} -> Mapped
    end.

% Function to get multiple values out of proplists
% The Description can be the Name of the property,
% a 2 element tuple with name and default {Name, Default}
% If only Name is given and the property is undefined then the key is added to
% MissingKeyList
mgetv(Descriptions, Proplist) ->
    mgetv(Descriptions, Proplist, [], []).
mgetv([{Name, Default} | Rest], Proplist, Acc, ErrAcc) ->
    mgetv(Rest , Proplist, [getv(Name, Proplist, Default) | Acc], ErrAcc);
mgetv([Name | Rest], Proplist, Acc, ErrAcc) ->
    case getv(Name, Proplist) of
        undefined ->
            mgetv(Rest , Proplist, [undefined | Acc], [Name | ErrAcc]);
        Value ->
            mgetv(Rest , Proplist, [Value | Acc], ErrAcc)
    end;
mgetv([], _, Acc, ErrAcc) -> {lists:reverse(Acc), lists:reverse(ErrAcc)}.

% Alternative getv. If a certain parameter can be in different places in the struct
% take the list of positions and return the first that is not undef.
agetv(Places, Proplist) -> agetv(Places, Proplist, undefined).
agetv([Place | Rest], Proplist, Default) ->
    case getv(Place, Proplist, Default) of
        undefined -> agetv(Rest, Proplist, Default);
        Value -> {Value, Place}
    end;
agetv([], _, _) -> {undefined, undefined}.

% Update multiple entries in a proplist
update([{_Key, undefined} |RestOfUpdates], Data) ->
    update(RestOfUpdates, Data);
update([{Key, Value} |RestOfUpdates], Data) ->
    update(RestOfUpdates, setv(Key, Value, Data));
update([], Result) -> Result.
