-module(url).
-export([parse/1]).

% -> {Proto,Host,Port,Path,Query}
%  | {error}
parse(S) ->
    {Proto, S1} = parse_proto(S),
    {Host, Port_, S2} = parse_host(S1),
    Port = case {Proto, Port} of
	       {"http", nil} ->
		   80;
	       {"https", nil} ->
		   443;
	       {_, P} ->
		   P
	   end,
    {Proto, Host, Port}.

parse_proto([C1, C2, C3 | S]) when [C1, C2, C3] == "://" ->
    {"", S};
parse_proto([C | S]) ->
    io:format("parse_proto(~s)~n", [[C | S]]),
    {C2, S2} = parse_proto(S),
    {[C | C2], S2}.

parse_host([C | S]) when [C] == ":" ->
    io:format("parse_host at colon~n", []),
    {PortS, S2} = parse_port(S),
    io:format("parse_port -> ~w~n", [PortS]),
    Port = list_to_integer(PortS),
    {"", Port, S2};
parse_host([C | S]) when [C] == "/" ->
    io:format("parse_host at slash~n", []),
    {"", nil, S};
parse_host([C | S]) ->
    io:format("parse_host(~s)~n", [[C|S]]),
    {C2, Port, S2} = parse_host(S),
    {[C | C2], Port, S2}.

parse_port([C | S]) when [C] == "/" ->
    {"", S};
parse_port([C | S]) ->
    io:format("parse_port(~s)~n", [[C|S]]),
    {C2, S2} = parse_port(S),
    {[C | C2], S2}.
