-module(update_job).

-export([start/1]).

%% for testing single feeds
-export([worker/1]).


-include("feed.hrl").

start(URL) ->
    spawn(?MODULE, worker, [URL]).

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
