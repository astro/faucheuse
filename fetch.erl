-module(fetch).
-export([run/0]).
-export([main/1]).

request_hostnames([]) ->
    done;
request_hostnames([H | T]) ->
    dns:lookup(H),
    request_hostnames(T).

run() ->
    %c(config),
    config:start(),
    %c(dns),
    dns:start(),
    %c(url),

    % {Url, true | false = ConnectionExists, [Hosts]}
    UrlConnections = ets:new(url_connections, [public, named_table]),
    lists:map(fun(Url) ->
		      ets:insert(UrlConnections,
				 {url:parse(Url), false, []})
	      end,
	      config:urls()),

% Estimate uniq list of Hostnames
    Hostnames = lists:usort(ets:foldl(fun({{_, Hostname, _, _, _}, _, _}, AccIn) ->
					      [Hostname | AccIn]
				      end,
				      [],
				      UrlConnections)),
    % Retrieve DNS
    request_hostnames(Hostnames),

    main(UrlConnections).

main(UrlConnections) ->
    %io:format("Urls: ~p~n", [UrlConnections]).

    receive
	{lookup, Name, Addresses} ->
	    io:format("~p is at ~p~n", [Name, Addresses]),
	    AddressUrls = ets:foldl(fun({{_, Hostname, _, _, _} = U, false, []}, AccIn)
				       when Hostname =:= Name ->
					    [U | AccIn];
				       (_, AccIn) ->
					    AccIn
				    end,
				    [],
				    UrlConnections),
	    lists:foldl(fun(nil, 0) ->
				0;
			   (Url, 0) ->
				io:format("~p -> ~p~n", [Url, Addresses]),
				ets:insert(UrlConnections, {Url, false, Addresses}),
				0
			end,
			0,
			AddressUrls)
    end,
    ?MODULE:main(UrlConnections).
