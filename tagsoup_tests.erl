-module(tagsoup_tests).

-export([run/0, test_chunksize/0]).

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
    test_pi(),
    test_chunksize().

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

test_chunksize() ->
    S = "<?xml version=\"1.0\" encoding=\"utf-8\"?>

<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title type=\"text\">dive into mark</title>
  <subtitle type=\"html\">
    A &lt;em&gt;lot&lt;/em&gt; of effort
    went into making this effortless
  </subtitle>

  <updated>2005-07-31T12:29:29Z</updated>
  <id>tag:example.org,2003:3</id>
  <link rel=\"alternate\" type=\"text/html\" 
   hreflang=\"en\" href=\"http://example.org/\"/>
  <link rel=\"self\" type=\"application/atom+xml\" 
   href=\"http://example.org/feed.atom\"/>
  <rights>Copyright (c) 2003, Mark Pilgrim</rights>

  <generator uri=\"http://www.example.com/\" version=\"1.0\">
    Example Toolkit
  </generator>
  <entry>
    <title>Atom draft-07 snapshot</title>
    <link rel=\"alternate\" type=\"text/html\" 
     href=\"http://example.org/2005/04/02/atom\"/>

    <link rel=\"enclosure\" type=\"audio/mpeg\" length=\"1337\"
     href=\"http://example.org/audio/ph34r_my_podcast.mp3\"/>
    <id>tag:example.org,2003:3.2397</id>
    <updated>2005-07-31T12:29:29Z</updated>
    <published>2003-12-13T08:29:29-04:00</published>

    <author>
      <name>Mark Pilgrim</name>
      <uri>http://example.org/</uri>
      <email>f8dy@example.com</email>

    </author>
    <contributor>
      <name>Sam Ruby</name>
    </contributor>
    <contributor>

      <name>Joe Gregorio</name>
    </contributor>
    <content type=\"xhtml\" xml:lang=\"en\" 
     xml:base=\"http://diveintomark.org/\">
      <div xmlns=\"http://www.w3.org/1999/xhtml\">
        <p><i>[Update: The Atom draft is finished.]</i></p>

      </div>
    </content>
  </entry>
</feed>",
    test_chunksize1(S, length(S), unknown).

test_chunksize1(_, 0, _) ->
    ok;

test_chunksize1(S, ChunkSize, Expected) ->
    R = spawn_link(fun receiver/0),
    {ok, P} = tagsoup_parser:start_link(
		fun(M) ->
			R ! {msg, M}
		end),
    test_chunksize2(S, ChunkSize, P),
    tagsoup_parser:stop(P),
    R ! {done, self()},
    receive
	{results, Results} ->
	    case Expected of
		unknown ->
		    test_chunksize1(S, ChunkSize - 1, Results);
		Results ->
		    test_chunksize1(S, ChunkSize - 1, Results);
		_ ->
		    error_logger:error_msg("Results:~n~p~nExpected:~n~p~n",
					   [Results, Expected]),
		    exit(fail)
	    end
    end.

test_chunksize2("", _, _) ->
    ok;
test_chunksize2(S, ChunkSize, P) ->
    {S1, S2} = if
		   ChunkSize < length(S) ->
		       lists:split(ChunkSize, S);
		   true ->
		       {S, ""}
	       end,
    tagsoup_parser:push(P, S1),
    test_chunksize2(S2, ChunkSize, P).
