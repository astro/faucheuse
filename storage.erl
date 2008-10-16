-module(storage).

-export([init/0, put_feed/3,
	 atomic/1, get_feed_by_url_t/1, get_entries_by_feed_url_t/1]).

-include("feed.hrl").

-record(feed_storage, {url,
		       feed}).
-record(entry_storage, {url_id, entry}).

init() ->
    mnesia:create_table(feed_storage, [%%{disc_copies, [node()]},
				       {attributes, record_info(fields, feed_storage)}]),
    mnesia:create_table(entry_storage, [%%{disc_copies, [node()]},
					{attributes, record_info(fields, entry_storage)}]),
    ok.

put_feed(URL, Feed, Entries) ->
    F = fun() ->
		mnesia:write(#feed_storage{url = URL,
					   feed = Feed}),
		lists:foreach(
		  fun(#entry{id = Id} = Entry) ->
			  mnesia:write(#entry_storage{url_id = {URL, Id},
						      entry = Entry})
		  end, Entries)
	end,
    {atomic, _} = mnesia:transaction(F).

atomic(Fun) ->
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_feed_by_url_t(URL) ->
    case mnesia:read(feed_storage, URL) of
	[#feed_storage{feed = Feed}] ->
	    Feed;
	_ ->
	    undefined
    end.
    

get_entries_by_feed_url_t(URL) ->
    mnesia:select(entry_storage, [{#entry_storage{url_id = {URL, '_'},
						    _ = '_'},
				     [], ['$_']}]).


