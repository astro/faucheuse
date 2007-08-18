-module(url).
-export([parse/1, string/1, test/0]).

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
	       {http, nil} ->
		   80;
	       {https, nil} ->
		   443;
	       {_, P} ->
		   P
	   end,
    io:format("S2: ~p~n", [S2]),
    {Path, Query} = case S2 of
			"" ->
			    {"/", ""};
			_ ->
			    parse_path(S2)
		    end,
    {Proto, Host, Port, Path, Query}.


parse_proto([$:, $/, $/ | S]) ->
    {"", S};
parse_proto([C | S]) ->
    {C2, S2} = parse_proto(S),
    {[C | C2], S2}.

% -> {Host, Port, Rest}
parse_host(S) ->
    case S of
	[$: | S2] ->
	    {PortS, S3} = parse_port(S2),
	    Port = list_to_integer(PortS),
	    {"", Port, S3};
	[$/ | _] ->
	    {"", nil, S};
	[$? | _] ->
	    {"", nil, [$/ | S]};
	[C | S2] ->
	    {C2, Port, S3} = parse_host(S2),
	    {[C | C2], Port, S3};
	[] ->
	    {"", nil, ""}
    end.

parse_port(S) ->
    io:format("parse_port(~s)~n", [S]),
    case S of
	[$/ | _] ->
	    {"", S};
	[$? | _] ->
	    {"", [$/ | S]};
	[C | S2] ->
	    {C2, S3} = parse_port(S2),
	    {[C | C2], S3};
	[] ->
	    {"", ""}
    end.

parse_path(S) ->
    io:format("parse_path(~s)~n", [S]),
    case S of
	[$? | S2] ->
	    {"", S2};
	[] ->
	    {"", ""};
	[C | S2] ->
	    {C2, S3} = parse_path(S2),
	    {[C | C2], S3}
    end.


       
string({Proto, Host, Port, Path, Query}) ->
    if
	is_atom(Proto) ->
	    atom_to_list(Proto);
	is_list(Proto) ->
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
	    [] ->
		"";
	    _ ->
		"?" ++ Query
	end.



test_url(S, Url) ->
    Url = parse(S),
    S = string(Url),
    ok.

test() ->
    % Empty path
    {http, "spaceboyz.net", 80, "/", ""} = parse("http://spaceboyz.net"),
    {http, "example", 80, "/", "test"} = parse("http://example?test"),
    {https, "example", 8443, "/", "test"} = parse("https://example:8443?test"),
    % Smoke
    test_url("http://spaceboyz.net/", {http, "spaceboyz.net", 80, "/", ""}),
    test_url("https://nsa.gov:8443/login?secure", {https, "nsa.gov", 8443, "/login", "secure"}).
