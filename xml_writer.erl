-module(xml_writer).

-export([to_string/1]).


to_string(XMLs) when is_list(XMLs) ->
    lists:flatten(lists:map(fun to_string/1, XMLs));

to_string(XML) ->
    lists:flatten(to_string1(XML)).

to_string1({Name, Attrs, Children}) ->
    [$<, Name,
     [[$ , K, $=, $",
       if
	   is_list(V) ->
	       entities:escape(V);
	   true ->
	       V2 = io_lib:format("~p", [V]),
	       entities:escape(V2)
       end,
       $"]
      || {K, V} <- Attrs],
     case Children of
	 [] -> "/>";
	 _ ->
	     [$>,
	      [to_string1(Child)
	       || Child <- Children],
	      $<, $/, Name, $>]
     end];

to_string1({text, S}) when is_list(S) ->
    entities:escape(S);

to_string1(L) when is_list(L) ->
    [S || S <- L];

to_string1(I) when is_integer(I) ->
    I;

to_string1(_) ->
    "".
