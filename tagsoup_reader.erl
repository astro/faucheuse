-module(tagsoup_reader).

-export([parse/1]).


parse(Text) ->
    I = self(),
    {ok, P} = tagsoup_parser:start_link(fun(Msg) ->
						I ! Msg
					end),
    tagsoup_parser:push(P, Text),
    I ! done,
    tagsoup_parser:stop(P),

    R = parse_listen([{root, [], []}]),
    lists:foldl(
      fun({root, _, Children}, Child) ->
	      lists:reverse([Child | Children]);
	 ({T1Name, T1Attrs, T1Children}, Child) ->
	      {T1Name, T1Attrs,
	       lists:reverse([Child | T1Children])}
      end, [], R).

parse_listen([Top | _] = Tree) ->
    receive
	{start_element, Name, Attributes} ->
	    parse_listen([{Name, Attributes, []} | Tree]);
	{end_element, Name} ->
	    Tree2 =
		lists:foldl(fun(T1Name, [{T1Name, T1Attrs, T1Children}, {T2Name, T2Attrs, T2Children} | Tree1]) ->
				    T1 = {T1Name, T1Attrs,
					  lists:reverse(T1Children)},
				    [{T2Name, T2Attrs, [T1 | T2Children]} | Tree1]
			    end, Tree, ending_elements(Name, Tree)),
	    parse_listen(Tree2);
	{text, Text} ->
	    {TName, TAttrs, TChildren} = Top,
	    [_ | Tree1] = Tree,
	    Tree2 = [{TName, TAttrs, [{text, Text} | TChildren]} | Tree1],
	    parse_listen(Tree2);
	done ->
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
