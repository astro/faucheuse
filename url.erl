-module(url).
-export([parse/1, string/1]).

% -> {Proto,Host,Port,Path,Query}
%  | {error}
parse(S) ->
    {Proto_, S1} = parse_proto(S),
    Proto = case Proto_ of
		"http" ->
		    http;
		"https" ->
		    https;
		_ ->
		    Proto_
	    end,
    {Host, Port_, S2} = parse_host(S1),
    Port = case {Proto, Port_} of
	       {http, false} ->
		   80;
	       {https, false} ->
		   443;
	       {_, P} ->
		   P
	   end,
    {Path, Query} = case S2 of
			"" ->
			    {"/", false};
			_ ->
			    parse_path(S2)
		    end,
    {Proto, Host, Port, Path, Query}.

parse_proto([C1, C2, C3 | S]) when [C1, C2, C3] == "://" ->
    {"", S};
parse_proto([C | S]) ->
    {C2, S2} = parse_proto(S),
    {[C | C2], S2}.

parse_host([C | S]) ->
    case [C] of
	":" ->
	    {PortS, S2} = parse_port(S),
	    Port = list_to_integer(PortS),
	    {"", Port, S2};
	"/" ->
	    {"", false, S};
	_->
	    {C2, Port, S2} = parse_host(S),
	    {[C | C2], Port, S2}
    end;
parse_host("") ->
    {"", false, ""}.

parse_port([C | S]) ->
    io:format("parse_port(~s)~n", [[C|S]]),
    case [C] of
	"/" ->
	    {"", S};
	_ ->
	    {C2, S2} = parse_port(S),
	    {[C | C2], S2}
    end;
parse_port([]) ->
    {"", ""}.

parse_path(S) ->
    case S of
	[C | S2] when [C] == "?" ->
	    {"", S2};
	[] ->
	    {"", false};
	[C | S2] ->
	    {C2, S3} = parse_path(S2),
	    {[C | C2], S3}
    end.
       
string({Proto, Host, Port, Path, Query}) ->
    if
	is_atom(Proto) ->
	    atom_to_list(Proto);
	is_list(proto) ->
	    Proto
    end ++
	"://" ++
	Host ++
	case {Proto, Port} of
	    {http, 80} ->
		"";
	    {https, 443} ->
		"";
	    {_, _} ->
		":" ++ integer_to_list(Port)
	end ++
	Path ++
	case Query of
	    false ->
		"";
	    _ ->
		"?" ++ Query
	end.
