-module(storage).

-export([init/0, put_feed/3]).

-include("feed.hrl").

-record(feed_storage, {url,
		       feed}).
-record(entry_storage, {url_id, entry}).

init() ->
    mnesia:create_table(feed_storage, [{attributes, record_info(fields, feed_storage)}]),
    mnesia:create_table(entry_storage, [{attributes, record_info(fields, entry_storage)}]),
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
