-module(config).
-export([start/0, start/1]).
-export([run/1]).

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
	{ok, Config} ->
	    case whereis(config) of
		undefined ->
		    ok;
		_ ->
		    io:format("config still runs, killing~n"),
		    exit(config, kill)
	    end,

	    Pid = spawn_link(?MODULE, run, [Config]),
	    register(config, Pid),
	    Pid
    end.

