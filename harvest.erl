-module(harvest).

-export([run/0, run/1]).


-include("feed.hrl").

run() ->
    run("harvester.cfg").

run(ConfigFile) ->
    harvester_sup:start_link(),
    storage:init(),
    config:start_link(ConfigFile),
    
    URLs = config:all_urls(),
    %%URLs = ["http://blog.fukami.io/feed/atom/"],
    process_flag(trap_exit, true),
    Workers = lists:map(fun(URL) ->
				Pid = spawn_link(fun() ->
							 worker(URL)
						 end),
				{URL, Pid}
			end, URLs),
    wait_workers(Workers),
    templates:run().

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
	    {ok, F} = feed_reader:start_link(URL),
	    (catch
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
				 end, 0)),
	    {Feed, Entries} = feed_reader:get_results(F),
	    EntriesS =
		lists:foldr(fun(#entry{title = Title, link = Link}, R) ->
				    [io_lib:format("~20s ~s~n",[Title, Link]) | R]
			    end, [], Entries),
	    error_logger:info_msg("*** ~p: ~s~n~s", [URL, Feed#feed.title, EntriesS]),
	    storage:put_feed(URL, Feed, Entries);
	_ ->
	    ignore
    end.
