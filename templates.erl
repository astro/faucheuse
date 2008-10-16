-module(templates).

-export([run/0, run/2]).


run() ->
    run("templates", "html").

run(TemplateDir, OutputDir) ->
    file:make_dir(OutputDir),
    storage:atomic(
      fun() ->
	      {ok, TemplateFiles} = file:list_dir(TemplateDir),
	      {ok, P} = erlxslt:start_link(),
	      erlxslt:set_xml(P, tagsoup_writer:to_string(generate_root())),
	      
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
    Collections = config:collections(),
    {"collections", [],
     lists:map(fun({CollectionName, Collection}) ->
		       {"collection", [{"name", CollectionName}],
			lists:map(fun(URL) ->
					  case storage:get_feed_by_url_t of
					      #feed{
					  [{"feed", [],
					    RSS ++
					    Title ++
					    Link ++
					    Description
				  end, Collection)}
	       end, Collections)}.
    
