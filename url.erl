-module(url).
-export([parse/1, get_path_query/1, to_string/1, join/2, test/0]).

-include("url.hrl").

parse(S) ->
    case string:str(S, ":") of
	Colon when Colon > 0 andalso Colon < 16 ->
	    URL = parse_scheme(#url{type = absolute}, S),
	    case URL#url.path of
		[] -> URL;
		[$/ | _] -> URL;
		Path -> URL#url{path = [$/ | Path]}
	    end;
	_ ->
	    parse_path(#url{type = relative}, S)
    end.

parse_scheme(URL, S) ->
    parse_scheme(URL, "", S).

parse_scheme(URL, Scheme, [$: | S]) ->
    parse_scheme_slashes(URL#url{scheme = list_to_atom(lists:reverse(Scheme))}, ":", S);

parse_scheme(URL, Scheme, [C | S]) ->
    parse_scheme(URL, [C | Scheme], S).

parse_scheme_slashes(URL, Sep, [$/ | S]) ->
    parse_scheme_slashes(URL, [$/ | Sep], S);
parse_scheme_slashes(URL, Sep, S) ->
    parse_host(URL#url{scheme_sep = lists:reverse(Sep)}, S).


parse_host(URL, S) ->
    parse_host(URL, "", S).

parse_host(URL, Host, []) ->
    URL#url{host = lists:reverse(Host),
	    port = scheme_default_port(URL#url.scheme)};

parse_host(URL, Host, [$: | S]) ->
    parse_port(URL#url{host = lists:reverse(Host)}, S);

parse_host(URL, Host, [$/ | S]) ->
    parse_path(URL#url{host = lists:reverse(Host),
		       port = scheme_default_port(URL#url.scheme)},
	       "/", S);

parse_host(URL, Host, [$? | S]) ->
    parse_q(URL#url{host = lists:reverse(Host),
		       port = scheme_default_port(URL#url.scheme),
		       path = ""}, S);

parse_host(URL, Host, [C | S]) ->
    parse_host(URL, [C | Host], S).


scheme_default_port(ftp) -> 21;
scheme_default_port(http) -> 80;
scheme_default_port(https) -> 443;
scheme_default_port(_) -> unknown.
     

parse_port(URL, S) ->
    parse_port(URL, "", S).

parse_port(URL, Port, []) ->
    {PortI, ""} = string:to_integer(lists:reverse(Port)),
    URL#url{port = PortI};

parse_port(URL, Port, [$/ | S]) ->
    {PortI, ""} = string:to_integer(lists:reverse(Port)),
    parse_path(URL#url{port = PortI},
	       "/", S);

parse_port(URL, Port, [$? | S]) ->
    {PortI, ""} = string:to_integer(lists:reverse(Port)),
    parse_q(URL#url{port = PortI,
		    path = ""}, S);

parse_port(URL, Port, [C | S]) ->
    parse_port(URL, [C | Port], S).

parse_path(URL, S) ->
    parse_path(URL, "", S).

parse_path(URL, Path, []) ->
    URL#url{path = lists:reverse(Path)};

parse_path(URL, Path, [$? | S]) ->
    parse_q(URL#url{path = lists:reverse(Path)}, S);

parse_path(URL, Path, [$# | S]) ->
    parse_fragment(URL#url{path = lists:reverse(Path)}, S);

parse_path(URL, Path, [C | S]) ->
    parse_path(URL, [C | Path], S).

parse_q(URL, S) ->
    parse_q(URL, "", S).

parse_q(URL, Q, []) ->
    URL#url{q = lists:reverse(Q)};

parse_q(URL, Q, [$# | S]) ->
    parse_fragment(URL#url{q = lists:reverse(Q)}, S);

parse_q(URL, Q, [C | S]) ->
    parse_q(URL, [C | Q], S).

parse_fragment(URL, S) ->
    URL#url{fragment = S}.

to_string(#url{scheme = Scheme,
	       scheme_sep = SchemeSep,
	       host = Host,
	       port = Port,
	       fragment = Fragment} = URL) ->
    DefaultPort = case (catch scheme_default_port(Scheme)) of
		      {'EXIT', _} -> unknown;
		      P -> P
		  end,
    if
	is_atom(Scheme) ->
	    atom_to_list(Scheme);
	is_list(Scheme) ->
	    Scheme
    end ++
	SchemeSep ++
	Host ++
	if
	    Port == DefaultPort ->
		"";
	    true ->
		":" ++ integer_to_list(Port)
	end ++
	get_path_query(URL) ++
	case Fragment of
	    [] ->
		"";
	    _ ->
		[$# | Fragment]
	end.

get_path_query(#url{path = Path,
		    q = Q}) ->
	case Path of
	    [] -> "";
	    [$/ | _] -> Path;
	    _ -> [$/ | Path]
	end ++
	case Q of
	    [] ->
		"";
	    _ ->
		[$? | Q]
	end.

join(U1, U2) when is_list(U1) ->
    join(parse(U1), U2);
join(U1, U2) when is_list(U2) ->
    join(U1, parse(U2));

join(_, #url{type = absolute} = U) ->
    U;

join(U, #url{path = "",
	     q = "",
	     fragment = Fragment}) ->
    U#url{fragment = Fragment};

join(U, #url{path = [$/ | _] = Path,
	     q = Q,
	     fragment = Fragment}) ->
    U#url{path = [$/ | _] = Path,
	  q = Q,
	  fragment = Fragment};

join(#url{path = Path1} = U, #url{path = Path2,
				 q = Q,
				 fragment = Fragment}) ->
    Path1E1 = tokenize(Path1, "/"),
    Path1E = lists:sublist(Path1E1, length(Path1E1) - 1),
    Path2E = tokenize(Path2, "/"),
    Path = case string:join(join_paths(lists:reverse(Path1E), Path2E), "/") of
	       [$/ | _] = P -> P;
	       P -> [$/ | P]
	   end,
    U#url{path = Path,
	  q = Q,
	  fragment = Fragment}.


join_paths(Path1, []) ->
    lists:reverse(Path1);

join_paths(Path1, ["." | Path2]) ->
    join_paths(Path1, Path2);

join_paths([], [".." | Path2]) ->
    join_paths([], Path2);
join_paths([_ | Path1], [".." | Path2]) ->
    join_paths(Path1, Path2);

join_paths(Path1, [Dir | Path2]) ->
    join_paths([Dir | Path1], Path2).


%% string:tokenize/2 returns [] for ("/", "/") instead of ["", ""]
tokenize(S, Tokens) ->
    tokenize(S, Tokens, [""]).

tokenize([], _, R) ->
    lists:reverse(R);

tokenize([T | S], Tokens, [R1 | R]) ->
    case lists:member(T, Tokens) of
	true ->
	    tokenize(S, Tokens, ["", R1 | R]);
	false ->
	    tokenize(S, Tokens, [R1 ++ [T] | R])
    end.


test_url(S, Url) ->
    Url = parse(S),
    S = to_string(Url),
    ok.

test() ->
    %% Empty path
    {url, absolute, http, "spaceboyz.net", 80, "", "", ""} = parse("http://spaceboyz.net"),
    {url, absolute, http, "example", 80, "", "test", ""} = parse("http://example?test"),
    {url, absolute, https, "example", 8443, "", "test", ""} = parse("https://example:8443?test"),
    %% Bidirectional
    test_url("http://spaceboyz.net/", {url, absolute, http, "spaceboyz.net", 80, "/", "", ""}),
    test_url("https://nsa.gov:8443/login?secure#form", {url, absolute, https, "nsa.gov", 8443, "/login", "secure", "form"}),
    %% Join
    {url, absolute, http, "spaceboyz.net", 80, "/~user/", "", ""} = join(parse("http://spaceboyz.net/~astro/foo/baz.html"), parse("./../../~user/")).

