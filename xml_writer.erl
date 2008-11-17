-module(xml_writer).

-export([to_string/1]).


-define(INDENTATION, 2).

to_string(XMLs) when is_list(XMLs) ->
    lists:flatten(lists:map(fun to_string/1, XMLs));

to_string(XML) ->
    lists:flatten(to_string1(XML, 0)).

to_string1({Name, Attrs, Children}, Indent) ->
    [make_whitespace(Indent), $<, Name,
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
	 [] -> "/>\n";
	 [{text, Text}] ->
	     %% One-text elements on one line
	     [$>, entities:escape(Text), $<, $/, Name, ">\n"];
	 _ ->
	     [">\n",
	      [to_string1(Child, Indent + ?INDENTATION)
	       || Child <- Children],
	      make_whitespace(Indent), "</", Name, ">\n"]
     end];

to_string1({text, S}, Indent) when is_list(S) ->
    make_whitespace(Indent) ++ entities:escape(S).

%%to_string1(L) when is_list(L) ->
%%    [S || S <- L];

%%to_string1(I) when is_integer(I) ->
%%    I;

%%to_string1(_) ->
%%    "".

make_whitespace(N) -> make_whitespace(N, "").
make_whitespace(0, R) -> R;
make_whitespace(N, R) -> make_whitespace(N - 1, [$  | R]).
