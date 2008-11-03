-module(templates).

-export([run/0, run/2]).


-include("feed.hrl").

-define(NS_HARVESTER, "http://astroblog.spaceboyz.net/harvester/xslt-functions").


run() ->
    run("templates", "html").

run(TemplateDir, OutputDir) ->
    file:make_dir(OutputDir),
    {ok, TemplateFiles} = file:list_dir(TemplateDir),
    {ok, P} = erlxslt:start_link("../erlang/erlxslt/erlxslt"),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "collection-items",
			      fun xslt_collection_items/1),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "collection-items",
			      fun xslt_collection_items/2),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "feed-items",
			      fun xslt_feed_items/1),
    erlxslt:register_function(P, ?NS_HARVESTER,
			      "feed-items",
			      fun xslt_feed_items/2),
io:format("r: ~s~n", [xml_writer:to_string(generate_root())]),
    erlxslt:set_xml(P, ".", xml_writer:to_string(generate_root())),
    
    lists:foreach(
      fun(TemplateFile) ->
	      {ok, Bin} = 
		  file:read_file(TemplateDir ++ "/" ++ TemplateFile),
	      erlxslt:set_xslt(P, ".", binary_to_list(Bin)),
	      
	      T1 = util:current_timestamp_ms(),
	      {ok, _, Output} = erlxslt:process(P),
	      io:format("XSLT: ~p us~n", [util:current_timestamp_ms() - T1]),
	      
	      file:write_file(OutputDir ++ "/" ++ TemplateFile,
			      Output)
      end, TemplateFiles),
    ok.

generate_root() ->
    {"collections", [],
     [{"collection", [{"name", atom_to_list(Name)}],
       [case storage:get_feed_by_url(URL) of
	    #feed{} = Feed ->
		feed_to_xml(URL, Feed);
	    _ -> []
	end
	|| URL <- URLs]}
      || {Name, URLs} <- config:collections()]}.

xslt_collection_items(Collection) ->
    xslt_collection_items(Collection, 23).

xslt_collection_items(Collection, Max) ->
    io:format("ci(~p)~n",[Collection]),
    CollectionURLs = case Collection of
			 "%" ->
			     config:all_urls();
			 _ ->
			     config:collection_urls(Collection)
		     end,
    Entries =
	lists:foldl(
	  fun(CollectionURL, Entries) ->
		  io:format("storage:get_entries_by_feed_url(~s)~n",[CollectionURL]),
		  Entries1 = storage:get_entries_by_feed_url(CollectionURL),
		  [{CollectionURL, Entry}
		   || Entry <- Entries1] ++ Entries
	  end, [], CollectionURLs),
    
    Entries2 = lists:reverse(lists:sort(fun({_, E1}, {_, E2}) ->
						compare_entries(E1, E2)
					end, Entries)),
    {Entries3, _} = util:split(Max, Entries2),
    {tree, xml_writer:to_string({"items", [],
				 [entry_to_xml(URL, Entry)
				  || {URL, Entry} <- Entries3]})}.

xslt_feed_items(URL) ->
    xslt_feed_items(URL, 23).

xslt_feed_items(URL, Max) ->
    io:format("xslt_feed_items(~p, ~p)~n",[URL, Max]),
    Entries = storage:get_entries_by_feed_url(URL),
    Entries2 = lists:reverse(lists:sort(fun compare_entries/2, Entries)),
    {Entries3, _} = util:split(Max, Entries2),
    io:format("~p entries for ~p~n",[length(Entries3), URL]),
    {tree, xml_writer:to_string({"items", [],
				 [entry_to_xml(URL, Entry)
				  || Entry <- Entries3]})}.

compare_entries(#entry{date = Date1}, #entry{date = Date2}) ->
    Date1 < Date2.

feed_to_xml(URL, Feed) ->
    {"feed", [],
     el_for_val("rss", URL) ++
     el_for_val("link", Feed#feed.link) ++
     el_for_val("title", Feed#feed.title) ++
     el_for_val("description", Feed#feed.description)
    }.

entry_to_xml(URL, Entry) ->
    {"item", [],
     el_for_val("rss", URL) ++
     el_for_val("id", Entry#entry.id) ++
     el_for_val("date", Entry#entry.date) ++
     el_for_val("title", Entry#entry.title) ++
     el_for_val("link", Entry#entry.link) ++
     el_for_val("description", Entry#entry.description)
    }.

el_for_val(_, undefined) ->
    [];
el_for_val(ElName, Value) when is_list(Value) ->
    [{ElName, [], [Value]}];
el_for_val(ElName, {{Y,Mo,D},{H,M,S}}) ->
    [{ElName, [],
      [lists:flatten(io_lib:format("~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
				   [Y,Mo,D,H,M,S]))]}];
el_for_val(ElName, Value) ->
    [{ElName, [], [io_lib:format("~p", [Value])]}].
