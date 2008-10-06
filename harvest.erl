-module(harvest).

-export([run/0, run/1]).


run() ->
    run("harvester.cfg").

run(ConfigFile) ->
    %% TODO: application, supervisor
    mnesia:start(),
    dns_cache:start_link(),
    http_client:start_link(),
    config:start_link(ConfigFile),
    
    URLs = config:all_urls(),
    %%URLs = ["http://chaosradio.ccc.de/everything.rss"],
    process_flag(trap_exit, true),
    Workers = lists:map(fun(URL) ->
				Pid = spawn_link(fun() ->
							 worker(URL)
						 end),
				{URL, Pid}
			end, URLs),
    wait_workers(Workers).

wait_workers([]) ->
    error_logger:info_msg("all workers done~n"),
    receive
	after 5000 ->
		done
	end;
wait_workers(Workers) ->
    error_logger:info_msg("~p workers to go~n", [length(Workers)]),
    receive
	{'EXIT', Pid, Reason} ->
	    {value, {URL, Pid}} = lists:keysearch(Pid, 2, Workers),
	    if
		Reason =/= normal ->
		    error_logger:error_msg("*** ~p (~p) exited with ~p",
					  [Pid, URL, Reason]);
		true -> no_output
	    end,
	    Workers2 = lists:keydelete(Pid, 2, Workers),
	    wait_workers(Workers2)
    end.

worker(URL) ->
    {response, Code, Status, _} = http_client:request(URL),
    error_logger:info_msg("*** ~p: ~p ~p", [URL, Code, Status]),
    {ok, Size} = http_client:recv(
	   fun(Data, Size1) ->
		   Size1 + length(Data)
	   end, 0),
    error_logger:info_msg("*** ~p: ~p bytes", [URL, Size]).
