-module(harvest).

-export([run/0, run/1]).

%% for testing single feeds
-export([worker/1]).


-include("feed.hrl").

run() ->
    run("harvester.cfg").

run(ConfigFile) ->
    application:start(sasl),
    harvester_sup:start_link(),
    config:start_link(ConfigFile),
    
    URLs = config:all_urls(),
    process_flag(trap_exit, true),
    Workers = lists:map(fun(URL) ->
				Pid = spawn_link(fun() ->
							 worker(URL)
						 end),
				{URL, Pid}
			end, URLs),
    wait_workers(Workers),
    %%templates:run().
    ok.

wait_workers([]) ->
    error_logger:info_msg("all workers done~n");

wait_workers(Workers) ->
    error_logger:info_msg("~p workers to go~n", [length(Workers)]),
    receive
	{'EXIT', Pid, Reason} ->
	    {value, {URL, Pid}} = lists:keysearch(Pid, 2, Workers),
	    if
		Reason =/= normal ->
		    error_logger:error_msg("*** ~p (~p) exited with ~p~n",
					  [Pid, URL, Reason]);
		true -> no_output
	    end,
	    Workers2 = lists:keydelete(Pid, 2, Workers),
	    wait_workers(Workers2)
    end.

worker(URL) ->
    Response = {response, Code, Status, _Headers} = http_client:request(URL),
    error_logger:info_msg("*** ~p: ~p ~p~n", [URL, Code, Status]),

    case Response of
	{response, 200, _, _}  ->
	    {ok, F} = feed_reader:start_link(URL,
					     fun(#feed{} = Feed) ->
						     storage:put_feed(URL, Feed);
						(#entry{} = Entry) ->
						     storage:put_entry(URL, Entry)
					     end),
	    {ok, _Size} = http_client:recv(
			    fun(Data, Size1) ->
				    Size2 = Size1 + length(Data),
				    if
					Size2 > 1024 * 1024 ->
					    exit(too_big);
					true ->
					    feed_reader:push(F, Data),
					    Size2
				    end
			    end, 0),
	    feed_reader:finish(F);
	_ ->
	    ignore
    end.
