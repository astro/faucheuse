-module(templates).

-export([run/0, run/2]).


-include("feed.hrl").

run() ->
    run("templates", "html").

run(TemplateDir, OutputDir) ->
    file:make_dir(OutputDir),
    storage:atomic(
      fun() ->
io:format("root: ~p~n",[generate_root()]),
	      {ok, TemplateFiles} = file:list_dir(TemplateDir),
	      {ok, P} = erlxslt:start_link("../erlang/erlxslt/erlxslt"),
	      erlxslt:set_xml(P, xml_writer:to_string(generate_root())),
	      
	      lists:foreach(
		fun(TemplateFile) ->
			{ok, Bin} = 
			    file:read_file(TemplateDir ++ "/" ++ TemplateFile),
			erlxslt:set_xslt(P, binary_to_list(Bin)),
			{ok, _, Output} = erlxslt:process(P),
			file:write_file(OutputDir ++ "/" ++ TemplateFile,
					Output)
		end, TemplateFiles)
      end),
    ok.

generate_root() ->
    {"collections", [],
     [{"collection", [{"name", atom_to_list(Name)}],
       [case storage:get_feed_by_url_t(URL) of
	    #feed{} = Feed ->
		feed_to_xml(URL, Feed);
	    _ -> []
	end
	|| URL <- URLs]}
      || {Name, URLs} <- config:collections()]}.

feed_to_xml(URL, Feed) ->
    {"feed", [],
     el_for_val("rss", URL) ++
     el_for_val("link", Feed#feed.link) ++
     el_for_val("title", Feed#feed.title) ++
     el_for_val("description", Feed#feed.description)
    }.

el_for_val(_, undefined) ->
    [];
el_for_val(ElName, Value) when is_list(Value) ->
    [{ElName, [], [Value]}].
