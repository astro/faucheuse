-module(tagsoup_tests).

-export([run/0]).

receiver() ->
    receiver([]).

receiver(Results) ->
    receive
	{msg, M} ->
	    receiver([M | Results]);
	{done, Caller} ->
	    Caller ! {results, lists:reverse(Results)}
    end.

assert(Buffer, Expected) ->
    R = spawn_link(fun receiver/0),
    {ok, P} = tagsoup_parser:start_link(
		fun(M) ->
			R ! {msg, M}
		end),
    tagsoup_parser:push(P, Buffer),
    tagsoup_parser:stop(P),
    R ! {done, self()},
    receive
	{results, Results} ->
	    if
		Results =:= Expected ->
		    ok;
		true ->
		    error_logger:error_msg("Expected: ~p~nActual: ~p~n",
					   [Expected, Results])
	    end
    end.

run() ->
    test_open_tag(),
    test_close_tag(),
    test_unquoted_attributes(),
    test_unquoted_attributes_slash(),
    test_unquoted_attributes_blank_end(),
    test_attributes_end(),
    test_comment(),
    test_text_tag_text_tag_text(),
    test_pi().

test_open_tag() ->
    assert("<HTML>", [{start_element, "HTML", []}]).

test_close_tag() ->
    assert("</HTML>", [{end_element, "HTML"}]).
    
test_unquoted_attributes() ->
    assert("<A HREF=about:blank>", [{start_element, "A", [{"HREF", "about:blank"}]}]).

test_unquoted_attributes_slash() ->
    assert("<A HREF=about:blank/>", [{start_element, "A", [{"HREF", "about:blank/"}]}]).

test_unquoted_attributes_blank_end() ->
    assert("<A HREF=about:blank />", [{start_element, "A", [{"HREF", "about:blank"}]},
				      {end_element, "A"}]).

test_attributes_end() ->
    assert("<a href='foo'/>", [{start_element, "a", [{"href", "foo"}]},
			       {end_element, "a"}]).

test_comment() ->
    assert("<!-- -->", []).

test_text_tag_text_tag_text() ->
    assert("foo<a>bar</a>baz", [{text, "foo"},
				{start_element, "a", []},
				{text, "bar"},
				{end_element, "a"}]).

test_pi() ->
    assert("<?xml version='1.0' encoding='UTF-8'?>", [{pi, "xml", [{"version", "1.0"},
								   {"encoding", "UTF-8"}]}]).
