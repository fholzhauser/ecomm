-module(ecomm_proto_sip).

-export([decode/1, decode_to_proplist/1, parse_multipart/1]).
-export([encode/1, encode_from_proplist/1]).

%%%==============================================
%%% Basic parser for SIP requests
%%%==============================================

-define(NL, <<"\r\n">>).     % New line
-define(BL, <<"\r\n\r\n">>). % Blank line
-define(ENC_MIME_BOUNDARY, <<"MIME_boundary_h7Fc5n9R02KqpLiJ6FN8">>).

decode(Message) when is_binary(Message) ->
    case decode_to_proplist(Message) of
	[_ | _] = Props ->
	    Get = fun (K) -> ecomm_util_proplist:getv(K, Props) end,
	    ReqVals = [Get(K) || K <- [[meta, method], [meta, uri], headers]],
	    ReqBody = case ecomm_util_proplist:mgetv([multipart, body], Props) of
			  {_, [multipart, body]} -> [undefined];
			  {[MultiPart, _], [body]} -> [MultiPart];
			  {[_, Body], [multipart]} -> [Body]
		      end,
	    Request = list_to_tuple([sip_request | ReqVals] ++ ReqBody),
	    case Get(next_request_chunk) of
		undefined -> {ok, Request};
		Next -> {ok, Request, Next}
	    end;
	Other ->
	    Other
    end.

decode_to_proplist(Message) when is_binary(Message) ->
    lists:foldl(fun (_F, Status) when is_atom(Status) -> 
			Status;
		    (F, State) -> 
			F(State)
		end, 
		Message,
		[fun split_message/1,
		 fun decode_method_uri/1,
		 fun decode_headers/1,
		 fun decode_body/1]).
		
split_message(Message) ->
    case binary:split(Message, ?NL) of
	[MethodUriChunk, Rest] ->
	    case binary:split(Rest, ?BL) of
		[HeadersChunk, BodyChunk] ->
		    [{chunks, [{method_uri, MethodUriChunk},
			       {headers, HeadersChunk}, 
			       {body, BodyChunk}]}];
		_ ->
		    incomplete
	    end;
	_ ->
	    incomplete
    end.

decode_method_uri(Props) ->
    MethodUriChunk = ecomm_util_proplist:getv([chunks, method_uri], Props),
    case binary:split(MethodUriChunk, <<" ">>, [global]) of
	[Method, Uri, <<"SIP/2.0">>] ->
	    ecomm_util_proplist:del([chunks, method_uri], Props) ++
		[{meta, [{method, Method}, {uri, Uri}]}];
	_ ->
	    {error, invalid_method_uri}
    end.
    
decode_headers(Props) ->
    HeadersChunk = ecomm_util_proplist:getv([chunks, headers], Props),
    ecomm_util_proplist:del([chunks, headers], Props) ++
	[{headers, parse_keyvals(HeadersChunk, <<": ">>)}].

decode_body(Props) ->
    BodyChunk = ecomm_util_proplist:getv([chunks, body], Props),
    case ecomm_util_proplist:getv([headers, <<"Content-Length">>], Props) of
	undefined ->
	    {error, no_content_length_header};
	ContentLengthChunk ->
	    CSize = binary_to_integer(ContentLengthChunk),
	    if CSize > size(BodyChunk) ->
		    incomplete;
	       true ->
		    <<ReqBody:CSize/binary, Next/binary>> = BodyChunk,
		    Bodies = decode_multipart_bodies(ReqBody),
		    ecomm_util_proplist:del([chunks], Props) ++ Bodies ++
			if size(Next) == 0 -> []; 
			   true -> [{next_request_chunk, Next}] 
			end
	    end
    end.


decode_multipart_bodies(<<"--MIME_boundary", _/binary>> = Payload) ->
    [Boundary, Rest] = binary:split(Payload, ?NL),
    Rest1 = binary:part(Rest, 0, size(Rest) - size(<<"--", ?NL/binary>>)), 
    PartBodies = binary:split(Rest1, Boundary, [global, trim]),
    [{multipart, [decode_multipart_body(PartBody) || PartBody <-  PartBodies]}];
decode_multipart_bodies(<<>>) ->
    [];
decode_multipart_bodies(Payload) ->
    [{body, Payload}].

decode_multipart_body(Body) ->
    [HeadersChunk, ContentChunk] = binary:split(Body, ?BL, [trim]),
    Headers = [parse_keyval(HeaderChunk, <<": ">>) ||
		  HeaderChunk <- binary:split(HeadersChunk, ?NL, [global]), 
		  HeaderChunk /= <<>>],
    {_, ContentType} = lists:keyfind(<<"Content-Type">>, 1, Headers),
    {ContentType, proplists:delete(<<"Content-Type">>, Headers), ContentChunk}.


%%%%%%%%%%

parse_multipart({<<"application/sdp">>, _, Body}) ->
    [parse_keyval(Line, <<"=">>) || Line <- binary:split(Body, ?NL, [global, trim])];
parse_multipart({<<"application/pidf+xml">>, _, Body}) ->
    ecomm_util_xml:parse(Body).


parse_keyval(Line, KVSeparator) ->
    list_to_tuple(binary:split(Line, KVSeparator)).
parse_keyvals(Chunk, KVSeparator) ->
    [parse_keyval(Line, KVSeparator) || Line <- binary:split(Chunk, ?NL, [global])].


%%%==============================================
%%% Basic encoder for SIP replies
%%%==============================================

encode({sip_reply, Status, Code, Headers}) 
  when is_binary(Status), is_integer(Code), is_list(Headers) ->
    encode({sip_reply, Status, Code, Headers, <<>>});

encode({sip_reply, Status, Code, Headers, MultiParts})
  when is_binary(Status), is_integer(Code), is_list(Headers), is_list(MultiParts) ->
    Body = << (<< <<(encode_multipart(MP))/binary>> || MP <- MultiParts >>)/binary, 
	      "--", ?ENC_MIME_BOUNDARY/binary, "--", ?NL/binary>>,
    encode({sip_reply, Status, Code, Headers, Body});
       
%% replaces Content-Type and Content-Length headers if present
encode({sip_reply, Status, Code, Headers, Body})
  when is_binary(Status), is_integer(Code), is_list(Headers), is_binary(Body) ->
    ContentType = <<"multipart/mixed;boundary=", ?ENC_MIME_BOUNDARY/binary>>,
    Headers1 = lists:foldl(fun ({K, V}, Acc) -> ecomm_util_proplist:setv(K, V, Acc) end,
			   Headers, 
			   [{<<"Content-Type">>, ContentType},
			    {<<"Content-Length">>, integer_to_binary(size(Body))}]),
    <<"SIP/2.0 ", (integer_to_binary(Code))/binary, " ", Status/binary, ?NL/binary,
      << <<K/binary, ": ", V/binary, ?NL/binary>> || {K, V} <- Headers1 >>/binary, ?NL/binary,
      Body/binary>>.
    

encode_from_proplist(Props) ->
    {[Status, Code, Headers], []} = ecomm_util_proplist:mgetv([status, code, headers], Props),
    encode({sip_reply, Status, Code, Headers,
	    case ecomm_util_proplist:mgetv([multipart, body], Props) of
		{_, [multipart, body]} -> <<>>;
		{[MultiPart, _], [body]} when is_list(MultiPart) -> MultiPart;
		{[_, Body], [multipart]} when is_binary(Body) -> Body
	    end}).


encode_multipart({ContentType, Headers, Body})
  when is_binary(ContentType), is_list(Headers), is_binary(Body) ->
    <<"--", ?ENC_MIME_BOUNDARY/binary, ?NL/binary, 
      "Content-Type: ", ContentType/binary, ?NL/binary,
      << <<K/binary, ": ", V/binary, ?NL/binary >> || {K, V} <- Headers >>/binary, ?NL/binary, 
      Body/binary, ?BL/binary>>.

