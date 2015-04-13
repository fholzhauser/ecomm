-module(ecomm_util_xml).
-export([
    parse/1, parse_simplified/1, simplify/1
]).

-include_lib("xmerl/include/xmerl.hrl").

%%%==============================================
%%% Helpers for XML
%%%==============================================

%% get XML tree as a list of nested xmerl records
parse(Bin) when is_binary(Bin) -> 
    parse(binary_to_list(Bin));
parse(Str) when is_list(Str) ->
    Acc = fun (#xmlText{value = " ", pos = P}, Acc, S) -> {Acc, P, S};
	      (#xmlComment{pos = P}, Acc, S) -> {Acc, P, S};
	      (X, Acc, S) -> {[X | Acc], S}
	  end,
    {Root, []} = xmerl_scan:string(Str, [{space,normalize}, {acc_fun, Acc}]),
    Root.

%% get XML tree as a list of nested tuples
parse_simplified(Xml) ->
    simplify(parse(Xml)).

%% convert xmerl records to tuples in XML tree
simplify(#xmlText{value = V}) -> V;
simplify(#xmlAttribute{name = K, value = V}) -> {K, V};
simplify(#xmlElement{name = K, attributes = Attrs, content = Body}) ->
    {K, [simplify(A) || A <- Attrs], [simplify(B) || B <- Body]}.

