-module(config).
-export([start/0, start/1, urls/0]).
-export([main/1]).


urls() ->
    config ! {self(), get_urls},
    receive
	{urls, Urls} ->
	    Urls
    end.


start() ->
    start("harvester.cfg").

start(Filename) ->
    %{ok, Config} = file:consult(Filename),
    case file:consult(Filename) of
	{error, {Line, erl_scan, scan}} ->
	    io:format("Syntax error in ~p, line ~p~n", [Filename, Line]),
	    exit(parse);
	{error, Reason} ->
	    io:format("Cannot open ~p: ~p~n", [Filename, Reason]),
	    exit(Reason);
	{ok, [Config | _]} ->
	    case whereis(config) of
		undefined ->
		    ok;
		_ ->
		    io:format("config still runs, killing~n"),
		    exit(config, kill)
	    end,

	    Pid = spawn_link(?MODULE, main, [Config]),
	    register(config, Pid),
	    Pid
    end.

main(Config) ->
    dbg:tpl(?MODULE,[{'_',[],[{message,{return_trace}}]}]),
  
    receive
	{Pid, get_urls} ->
	    io:format("get_urls from ~p~n", [Pid]),
	    io:format("Config: ~p~n", [Config]),
	    {value, {collections, Collections}} = lists:keysearch(collections, 1, Config),
	    io:format("Collections: ~p~n", [Collections]),
	    Urls = lists:usort(
		     % flatten with depth = 1
		     lists:foldl(fun({_, UrlList}, AccIn) ->
					 AccIn ++ UrlList
				 end,
				 [],
				 Collections) ),
	    Pid ! {urls, Urls}
    end,
    ?MODULE:main(Config).

    
