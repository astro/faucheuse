-module(storage).

-export([init/0, put_feed/2, put_entry/2,
	 get_feed_by_url/1, get_entries_by_feed_url/1]).

-include("feed.hrl").

-record(feed_storage, {url,
		       feed}).
-record(entry_storage, {url_id, entry}).

init() ->
    mnesia:create_table(feed_storage, [{disc_copies, [node()]},
				       {attributes, record_info(fields, feed_storage)}]),
    mnesia:create_table(entry_storage, [{disc_copies, [node()]},
					{attributes, record_info(fields, entry_storage)}]),
    ok.

put_feed(URL, Feed) ->
    F = fun() ->
		mnesia:write(#feed_storage{url = URL,
					   feed = Feed})
	end,
    {atomic, _} = mnesia:transaction(F).

put_entry(URL, #entry{id = Id} = Entry) ->
    F = fun() ->
		mnesia:write(#entry_storage{url_id = {URL, Id},
					    entry = Entry})
	end,
    {atomic, _} = mnesia:transaction(F).
    

atomic(Fun) ->
    case mnesia:transaction(Fun) of
	{atomic, Res} ->
	    Res;
	{aborted, Reason} ->
	    error_logger:error_msg("storage:atomic: ~p~n", [Reason]),
	    exit(aborted)
    end.

get_feed_by_url(URL) ->
    atomic(fun() ->
		   case mnesia:read({feed_storage, URL}) of
		       [#feed_storage{feed = Feed}] ->
			   Feed;
		       _ ->
			   undefined
		   end
	   end).
    

get_entries_by_feed_url(URL) ->
    atomic(fun() ->
		   [Entry
		    || #entry_storage{entry = Entry} <- mnesia:select(entry_storage, [{#entry_storage{url_id = {URL, '_'},
												      _ = '_'},
										       [], ['$_']}])]
	   end).


