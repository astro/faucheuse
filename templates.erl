-module(templates).

-export([start_link/1, process/2]).


-include("feed.hrl").

-define(NS_HARVESTER, "http://astroblog.spaceboyz.net/harvester/xslt-functions").
-define(NS_XHTML, "http://www.w3.org/1999/xhtml").

-define(UTF8(S), "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++ S).

start_link(Collections) ->
    {ok, P} = erlxslt:start_link("../erlang/erlxslt/erlxslt"),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "collection-items",
			      fun() ->
				      URLs =
					  lists:foldl(
					    fun({_, CollectionURLs}, R) ->
						    CollectionURLs ++ R
					    end, [], Collections),
				      xslt_collection_items(URLs)
			      end),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "collection-items",
			      fun(Collection) ->
				      {value, {_, URLs}} =
					  lists:keysearch(list_to_atom(Collection), 1, Collections),
				      xslt_collection_items(URLs)
			      end),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "collection-items",
			      fun(Collection, Max) ->
				      {value, {_, URLs}} =
					  lists:keysearch(list_to_atom(Collection), 1, Collections),
				      xslt_collection_items(URLs, Max)
			      end),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "feed-items",
			      fun xslt_feed_items/1),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "feed-items",
			      fun xslt_feed_items/2),
    erlxslt:set_xml(P, "collections.xml",
		    ?UTF8(xml_writer:to_string(generate_root(Collections)))),
    {ok, P}.

process(P, XsltFile) ->
    io:format("reading xslt file ~p~n",[XsltFile]),
    {ok, Bin} = 
	file:read_file(XsltFile),
    erlxslt:set_xslt(P, XsltFile, binary_to_list(Bin)),

    T1 = util:current_timestamp_ms(),
    {ok, Type, Output} = erlxslt:process(P),
    io:format("XSLT: ~p us~n", [util:current_timestamp_ms() - T1]),
	      
    {ok, Type, Output}.

generate_root(Collections) ->
    {"collections", [],
     [{"collection", [{"name", atom_to_list(Name)}],
       [feed_to_xml(URL, Feed)
	|| {URL, Feed} <- [{URL, storage:get_feed_by_url(URL)}
			   || URL <- URLs],
	   element(1, Feed) =:= feed]}
      || {Name, URLs} <- Collections]}.

xslt_collection_items(CollectionURLs) ->
    xslt_collection_items(CollectionURLs, 23).

xslt_collection_items(CollectionURLs, Max) ->
    Entries =
	lists:foldl(
	  fun(CollectionURL, Entries) ->
		  %%io:format("storage:get_entries_by_feed_url(~s)~n",[CollectionURL]),
		  Entries1 = storage:get_entries_by_feed_url(CollectionURL),
		  [{CollectionURL, Entry}
		   || Entry <- Entries1] ++ Entries
	  end, [], CollectionURLs),
    
    Entries2 = lists:reverse(lists:sort(fun({_, E1}, {_, E2}) ->
						compare_entries(E1, E2)
					end, Entries)),
    {Entries3, _} = util:split(Max, Entries2),
    {tree, ?UTF8(xml_writer:to_string({"items", [],
				       [entry_to_xml(URL, Entry)
					|| {URL, Entry} <- Entries3]}))}.

xslt_feed_items(URL) ->
    xslt_feed_items(URL, 23).

xslt_feed_items(URL, Max) ->
    %%io:format("xslt_feed_items(~p, ~p)~n",[URL, Max]),
    Entries = storage:get_entries_by_feed_url(URL),
    Entries2 = lists:reverse(lists:sort(fun compare_entries/2, Entries)),
    {Entries3, _} = util:split(Max, Entries2),
    {tree, ?UTF8(xml_writer:to_string({"items", [],
				       [entry_to_xml(URL, Entry)
					|| Entry <- Entries3]}))}.

compare_entries(#entry{date = Date1}, #entry{date = Date2}) ->
    Date1 < Date2.

feed_to_xml(URL, Feed) ->
    {"feed", [],
     el_for_val("rss", URL) ++
     el_for_val("link", Feed#feed.link) ++
     el_for_val("title", Feed#feed.title) ++
     [{"description", [{"xmlns", ?NS_XHTML}], Feed#feed.description}] %% TODO: text? markup!
    }.

entry_to_xml(URL, Entry) ->
    {"item", [],
     el_for_val("rss", URL) ++
     el_for_val("id", Entry#entry.id) ++
     el_for_val("date", Entry#entry.date) ++
     el_for_val("title", Entry#entry.title) ++
     el_for_val("link", Entry#entry.link) ++
     [{"description", [{"xmlns", ?NS_XHTML}], Entry#entry.description}]
    }.

el_for_val(_, undefined) ->
    [];
el_for_val(ElName, Value) when is_list(Value) ->
    [{ElName, [], [{text, Value}]}];
el_for_val(ElName, {{Y,Mo,D},{H,M,S}}) ->
    [{ElName, [],
      [{text, lists:flatten(io_lib:format("~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
					  [Y,Mo,D,H,M,S]))}]}];
el_for_val(ElName, Value) ->
    [{ElName, [], [{text, io_lib:format("~p", [Value])}]}].
