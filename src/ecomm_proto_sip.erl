-module(ecomm_proto_sip).

-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).

-record(sip_invite,
	{uri,
	 to,
	 from,
	 vias = [],
	 routes = [],
	 record_routes = [],
	 max_forwards,
	 cseq,
	 call_id,
	 session_id,
	 session_expires,
	 contacts = [],
	 supported = [],
	 headers = [],
	 sdp_kvs = [],
	 pidf_kvs = []}).


%%%%%%%%%%

decode(Packet) when is_binary(Packet) -> 
    {{MetaChunk, HeadersChunk, DataChunk}, EndOfLine} = split_packet(Packet),
    Meta = parse_meta(MetaChunk),
    Headers = parse_headers(HeadersChunk, EndOfLine),
    Data = parse_data(DataChunk, EndOfLine),
    sip_record(Meta, Headers, Data).


split_packet(Packet) ->
    {match, [{MethodUriEnd, EolSz}]} = re:run(Packet, "\r?\n"),
    {match, [{HeadersEnd, BlankSz}]} = re:run(Packet, "\r?\n\r?\n", [{offset, MethodUriEnd}]),
    {{binary:part(Packet, 0, MethodUriEnd),
      binary:part(Packet, MethodUriEnd + EolSz, HeadersEnd - MethodUriEnd - EolSz),
      binary:part(Packet, HeadersEnd + BlankSz, size(Packet) - HeadersEnd - BlankSz - EolSz)},
     case EolSz of
	 1 -> <<"\n">>;
	 2 -> <<"\r\n">>
     end}.


parse_meta(Chunk) ->
    [Method, Uri, <<"SIP/2.0">>] = binary:split(Chunk, <<" ">>, [global]),
    {list_to_atom(string:to_lower(binary_to_list(Method))), Uri}.

parse_headers(Chunk, EndOfLine) ->
    parse_keyvals(Chunk, <<": ">>, EndOfLine).

parse_data(Chunk, EndOfLine) ->
    {match, [{BoundarySz, _}]} = re:run(Chunk, EndOfLine),
    Boundary = binary:part(Chunk, 0, BoundarySz),
    case binary:matches(Chunk, Boundary) of
	[] -> [];
	[{0, _}] -> [];
	[{0, Start} | Positions] ->
	    EolSz = size(EndOfLine),
	    {_, Parts} =
		lists:foldl(
		  fun ({Pos, _}, {Cursor, Parts}) ->
			  PartChunk = binary:part(Chunk, Cursor, Pos - Cursor),
			  Part = parse_data_part(PartChunk, EndOfLine, EolSz),
			  {Pos + BoundarySz, [Part | Parts]}
		  end, {Start, []}, Positions),
	    lists:reverse(Parts)
    end.


parse_data_part(Chunk, EndOfLine, EolSz) ->
    {match, [{HeadersEnd, BlankSz}]} = re:run(Chunk, "\r?\n\r?\n"),
    HeadersChunk = binary:part(Chunk, EolSz, HeadersEnd - EolSz),
    Headers = parse_headers(HeadersChunk, EndOfLine),
    Body = binary:part(Chunk, HeadersEnd + BlankSz, size(Chunk) - HeadersEnd - (2 * BlankSz)),
    {_, MIMEType} = lists:keyfind(<<"Content-Type">>, 1, Headers),
    {Headers, parse_data_part_body(MIMEType, Body, EndOfLine)}.

parse_data_part_body(<<"application/sdp">>, Chunk, EndOfLine) ->
    [{binary_to_atom(K, utf8), V}  || {K, V} <- parse_keyvals(Chunk, <<"=">>, EndOfLine)];
parse_data_part_body(<<"application/pidf+xml">>, Chunk, _EndOfLine) ->
    XML = xmlread_simple(Chunk),
    {presence, _, 
     [{'dm:device', [{id, DeviceId}],
       [{'gp:geopriv', [],
	 [{'gp:location-info', [],
	   [{'gs:Circle', [{srsName, "urn:ogc:def:crs:EPSG::4326"}],
	     [{'gml:pos', [],
	       [Position]},
	      {'gs:radius', [{uom,"urn:ogc:def:uom:EPSG::9001"}],
	       [Radius]}]},
	    {'con:confidence', [{pdf,"normal"}],
	     [Confidence]}]},
	  {'gp:usage-rules', [], []}]},
	{'dm:timestamp', [], [Timestamp]}]}]} = XML,
    [{device_id, DeviceId}, 
     {position, Position}, {radius, Radius}, {confidence, Confidence},
     {timestamp, Timestamp}].

%%

parse_sip_address(X) -> 
    X.
     
parse_header(<<"Max-Forwards">>, Val) ->
    binary_to_integer(Val);
parse_header(<<"Session-Expires">>, Val) ->
    binary_to_integer(Val);
parse_header(<<"To">>, Val) ->
    parse_sip_address(Val);
parse_header(<<"From">>, Val) ->
    parse_sip_address(Val);
parse_header(<<"Via">>, Val) ->
    parse_sip_address(Val);
parse_header(<<"Route">>, Val) ->
    parse_sip_address(Val);
parse_header(<<"Record-Route">>, Val) ->
    parse_sip_address(Val);

parse_header(_, Val) ->
    Val.

%%%%%%%%%%

sip_record({invite, UriChunk}, Headers, Data) ->
    GetHeader  = fun (K) -> {_, V} = lists:keyfind(K, 1, Headers), parse_header(K, V) end,
    GetHeaders = fun (K) -> [parse_header(K, V) || {_, V} <- proplists:lookup_all(K, Headers)] end,
    SingleKeys = [<<"Max-Forwards">>, <<"To">>, <<"From">>, <<"Call-ID">>, <<"CSeq">>,
		  <<"Session-ID">>, <<"Session-Expires">>], 
    MultiKeys  = [<<"Via">>, <<"Contact">>, <<"Route">>, <<"Record-Route">>, <<"Supported">>],
    [MaxForwards, To, From, CallID, CSeq, SessionID, SessionExp] = [GetHeader(K) || K <- SingleKeys],
    [Vias, Contacts, Routes, RecRoutes, Supps] = [GetHeaders(K) || K <- MultiKeys],
    RemHeaders = lists:foldl(fun (Ks, KVs) -> lists:foldl(fun proplists:delete/2, KVs, Ks) end,
			     Headers, [SingleKeys, MultiKeys]),
    [{[{<<"Content-Type">>, <<"application/sdp">>} | _], SDP}, 
     {[{<<"Content-Type">>, <<"application/pidf+xml">>} | _], PIDF}] = Data,
    #sip_invite{uri = UriChunk,
		to = To,
		from = From,
		vias = Vias,
		routes = Routes,
		record_routes = RecRoutes,
		max_forwards = MaxForwards,
		cseq = CSeq,
		call_id = CallID,
		session_id = SessionID,
		session_expires = SessionExp,
		contacts = Contacts,
		supported = Supps,
		headers = RemHeaders,
		sdp_kvs = SDP,
		pidf_kvs = PIDF}.
		
       
%%%%%%%%%%

xmerl_scan_acc(#xmlText{value = " ", pos = P}, Acc, S) -> {Acc, P, S};
xmerl_scan_acc(#xmlComment{pos = P}, Acc, S) -> {Acc, P, S};
xmerl_scan_acc(X, Acc, S) -> {[X | Acc], S}.

xmerl_simplify(#xmlText{value = V}) -> V;
xmerl_simplify(#xmlAttribute{name = K, value = V}) -> {K, V};
xmerl_simplify(#xmlElement{name = K, attributes = Attrs, content = Body}) ->
    {K, [xmerl_simplify(A) || A <- Attrs], [xmerl_simplify(B) || B <- Body]}.

xmlread(Bin) when is_binary(Bin) -> xmlread(binary_to_list(Bin));
xmlread(Str) when is_list(Str) ->
    {Root, []} = xmerl_scan:string(Str, [{space,normalize}, {acc_fun, fun xmerl_scan_acc/3}]),
    Root.

xmlread_simple(Xml) ->
    xmerl_simplify(xmlread(Xml)).


parse_keyvals(Chunk, KVSeparator, EndOfLine) ->
    [list_to_tuple(binary:split(Line, KVSeparator)) ||
	Line <- binary:split(Chunk, EndOfLine, [global])].
