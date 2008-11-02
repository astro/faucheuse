-module(tagsoup_reader).

-export([parse/1]).


%% Parse XML and return a tree
parse(Text) ->
    I = self(),
    Ref = make_ref(),
    {ok, P} = tagsoup_parser:start_link(fun(Msg) ->
						I ! {Ref, Msg}
					end),
    tagsoup_parser:push(P, Text),
    tagsoup_parser:stop(P),
    I ! {Ref, done},

    R = parse_listen(Ref, [{root, [], []}]),
    lists:foldl(
      fun({root, _, Children}, none) ->
	      lists:reverse(Children);
	 ({root, _, Children}, Child) ->
	      lists:reverse([Child | Children]);
	 ({T1Name, T1Attrs, T1Children}, none) ->
	      {T1Name, T1Attrs,
	       lists:reverse(T1Children)};
	 ({T1Name, T1Attrs, T1Children}, Child) ->
	      {T1Name, T1Attrs,
	       lists:reverse([Child | T1Children])}
      end, none, R).

parse_listen(Ref, [Top | _] = Tree) ->
    receive
	{Ref, {start_element, Name, Attributes}} ->
	    parse_listen(Ref, [{Name, Attributes, []} | Tree]);
	{Ref, {end_element, Name}} ->
	    Tree2 =
		lists:foldl(fun(T1Name, [{T1Name, T1Attrs, T1Children}, {T2Name, T2Attrs, T2Children} | Tree1]) ->
				    T1 = {T1Name, T1Attrs,
					  lists:reverse(T1Children)},
				    [{T2Name, T2Attrs, [T1 | T2Children]} | Tree1]
			    end, Tree, ending_elements(Name, Tree)),
	    parse_listen(Ref, Tree2);
	{Ref, {text, Text}} ->
	    {TName, TAttrs, TChildren} = Top,
	    [_ | Tree1] = Tree,
	    Tree2 = [{TName, TAttrs, [{text, Text} | TChildren]} | Tree1],
	    parse_listen(Ref, Tree2);
	{Ref, done} ->
	    Tree
    end.

ending_elements(Name, Tree) ->
    ending_elements(Name, Tree, []).

ending_elements(_Name, [], _) ->
    [];
ending_elements(Name, [{Name, _, _} | _], R) ->
    [Name | R];
ending_elements(Name, [{Name1, _, _} | Tree], R) ->
    ending_elements(Name, Tree, [Name1 | R]).
