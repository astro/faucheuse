-module(storage).

-export([init/0, put_feed/2, put_entry/2,
	 get_feed_by_url/1, get_entries_by_feed_url/1]).

-include("feed.hrl").

-record(feed_storage, {url, feed, last_seen=0}).
-record(entry_storage, {url_id, entry, last_seen=0}).

init() ->
    mnesia:create_table(feed_storage, [{disc_copies, [node()]},
				       {attributes, record_info(fields, feed_storage)}]),
    mnesia:create_table(entry_storage, [{disc_copies, [node()]},
					{attributes, record_info(fields, entry_storage)}]),
    ok.

put_feed(URL, Feed) ->
    F = fun() ->
		mnesia:write_lock_table(feed_storage),
		IsNew = (mnesia:read({feed_storage, URL}) =:= []),
		mnesia:write(#feed_storage{url = URL,
					   feed = Feed,
					   last_seen = util:current_timestamp()}),
		IsNew
	end,
    {atomic, IsNew} = mnesia:transaction(F),
    if
	IsNew ->
	    notify:notify(URL, {feed_new, Feed});
	true ->
	    notify:notify(URL, {feed_updated, Feed})
    end.

put_entry(URL, #entry{id = Id} = Entry) ->
    F = fun() ->
		mnesia:write_lock_table(entry_storage),
		%% Never apply new timestamps
		{NewEntry, IsNew} =
		    case mnesia:read({entry_storage, {URL, Id}}) of
			[] -> {Entry, true};
			[#entry_storage{entry = #entry{date = OldDate}}] ->
			    {Entry#entry{date = OldDate}, false}
		    end,
		mnesia:write(#entry_storage{url_id = {URL, Id},
					    entry = NewEntry,
					    last_seen = util:current_timestamp()}),
		IsNew
	end,
    {atomic, IsNew} = mnesia:transaction(F),
    if
	IsNew ->
	    notify:notify(URL, {entry_new, Entry});
	true ->
	    notify:notify(URL, {entry_updated, Entry})
    end.

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


