-module(feed_reader).

-behaviour(gen_server).

%% API
-export([start_link/0, push/2, get_results/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("feed.hrl").

-record(state, {parser,
		stack = [],
		record = false, %% false | text | tree
		current = "",
		feed = #feed{},
		entries = []
	       }).

%% TODO: atom @type, xml:base

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

push(Pid, Data) ->
    gen_server:cast(Pid, {push, Data}).

get_results(Pid) ->
    gen_server:call(Pid, get_results).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    I = self(),
    {ok, Parser} = tagsoup_parser:start_link(
		     fun(Msg) ->
			     gen_server:cast(I, Msg)
		     end),
    {ok, #state{parser = Parser}}.

handle_call(get_results, _From, #state{parser = none} = State) ->
    %% TODO: id-ify
    Reply = {State#state.feed,
	     lists:reverse(State#state.entries)},
    {reply, Reply, State};

handle_call(get_results, From, #state{parser = Parser} = State) ->
    tagsoup_parser:stop(Parser),
    handle_call(get_results, From, State#state{parser = none}).


%% push

handle_cast({push, Data}, #state{parser = Parser} = State) ->
    tagsoup_parser:push(Parser, Data),
    {noreply, State};


%% tagsoup callbacks

handle_cast({start_element, Name, Attributes}, #state{stack = Stack} = State) ->
    Name1 = mangle_element_name(Name),
    State2 = start_element(State#state{stack = [Name1 | Stack]}, Attributes),
    if
	State#state.record =/= State2#state.record ->
	    {noreply, State2#state{current = ""}};
	true ->
	    {noreply, State2}
    end;

handle_cast({end_element, Name}, #state{stack = Stack, current = Current} = State) ->
    Name1 = mangle_element_name(Name),
    State2 = State#state{current = util:string_chomp(Current)},
    case end_element_in_stack(Name1, Stack) of
	undefined ->
	    {noreply, State2};
	{NewStack, OldStacks} ->
	    State3 =
		lists:foldl(
		  fun(OldStack, State1) ->
			  end_element(State1#state{stack = OldStack})
		  end, State2, OldStacks),
	    {noreply, State3#state{stack = NewStack}}
    end;

handle_cast({text, Text}, #state{record = text, current = Current} = State) ->
    {noreply, State#state{current = Current ++ Text}};

handle_cast({text, _}, #state{record = false} = State) ->
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.
		      

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

mangle_element_name(Name) ->
    Name2 = string:to_lower(Name),
    case string:chr(Name2, $:) of
	0 ->
	    Name2;
	N ->
	    string:substr(Name2, N + 1)
    end.

get_attr_s(Name, Attributes) ->
    case lists:keysearch(Name, 1, Attributes) of
	{value, {_, Value}} ->
	    Value;
	false ->
	    ""
    end.

%% returns a list of stacks for each ended element
end_element_in_stack(Name, Stack) ->
    end_element_in_stack(Name, Stack, []).

end_element_in_stack(Name, [Name | Stack1] = Stack, EndedStacks) ->
    {Stack1, lists:reverse([Stack | EndedStacks])};
end_element_in_stack(Name, [_ | Stack1] = Stack, Ended) ->
    end_element_in_stack(Name, Stack1, [Stack | Ended]);
end_element_in_stack(_, [], _) ->
    undefined.


start_element(#state{stack = [Format]} = State, _) ->
    case Format of
	"feed" -> ok;
	"rss" -> ok;
	"rdf" -> ok;
	_ -> exit(unsupported_feed_format)
    end,
    State#state{stack = [Format]};

%% RSS & RDF

start_element(#state{stack = Stack} = State, _)
  when tl(Stack) =:=  ["item", "channel", "rss"];
       tl(Stack) =:= ["item", "rdf"] ->
    State#state{record = text};

start_element(#state{stack = Stack,
		     entries = Entries} = State, _)
  when Stack =:=  ["item", "channel", "rss"];
       Stack =:= ["item", "rdf"] ->
    State#state{record = false, entries = [#entry{} | Entries]};

start_element(#state{stack = [_, "channel", _]} = State, _) ->
    State#state{record = text};

%% ATOM

start_element(#state{stack = ["link", "feed"],
		     feed = Feed} = State, Attr) ->
    Href = get_attr_s("href", Attr),
    Rel = get_attr_s("rel", Attr),
    NewLink = case {Feed#feed.link, Rel} of
		  %% Get at least one <link/>
		  {undefined, _} -> Href;
		  %% but prefer <link rel="alternate"/>
		  {_, "alternate"} -> Href;
		  %% or keep the previous one
		  {OldLink, _} -> OldLink
	      end,
    State#state{record = false,
		feed = Feed#feed{link = NewLink}};

start_element(#state{stack = ["link", "entry", "feed"],
		     entries = [Entry | Entries]} = State, Attr) ->
    Href = get_attr_s("href", Attr),
    Rel = get_attr_s("rel", Attr),
    NewLink = case {Entry#entry.link, Rel} of
		  %% Get at least one <link/>
		  {undefined, _} -> Href;
		  %% but prefer <link rel="alternate"/>
		  {_, "alternate"} -> Href;
		  %% or keep the previous one
		  {OldLink, _} -> OldLink
	      end,
    State#state{record = false,
		entries = [Entry#entry{link = NewLink} | Entries]};

start_element(#state{stack = [_, "entry", "feed"]} = State, _) ->
    State#state{record = text};

start_element(#state{stack = ["entry", "feed"],
		     entries = Entries} = State, _) ->
    State#state{record = false, entries = [#entry{} | Entries]};

start_element(#state{stack = [_, "feed"]} = State, _) ->
    State#state{record = text};

%% Fall-through

start_element(State, _) ->
    State.


%% RSS

end_element(#state{stack = [Tag, "channel", _],
		   current = Current,
		   feed = Feed} = State) ->
    State#state{record = false,
		feed = rss_channel(Feed, Tag, Current)};

end_element(#state{stack = [Tag | Stack],
		   current = Current,
		   entries = [Entry | Entries]} = State)
  when Stack =:= ["item", "channel", "rss"];
       Stack =:= ["item", "rdf"] ->
    State#state{record = false,
		entries = [rss_channel_item(Entry, Tag, Current) | Entries]};

end_element(#state{stack = [_, "channel", _]} = State) ->
    State#state{record = false};

%% ATOM

end_element(#state{stack = [Tag, "feed"],
		   current = Current,
		   feed = Feed} = State) ->
io:format("atom_feed ~p = ~p~n",[Tag, Current]),
    State#state{record = false,
		feed = atom_feed(Feed, Tag, Current)};

end_element(#state{stack = [Tag, "entry", "feed"],
		   current = Current,
		   entries = [Entry | Entries]} = State) ->
io:format("atom_entry ~p = ~p~n",[Tag, Current]),
    State#state{record = false,
		entries = [atom_entry(Entry, Tag, Current) | Entries]};


%% Fall-through

end_element(State) ->
    State.


%% RSS

rss_channel(Feed, "title", Text) ->
    Feed#feed{title = Text};
rss_channel(Feed, "link", Text) ->
    Feed#feed{link = Text};
rss_channel(Feed, "description", Text) ->
    Feed#feed{description = Text};
rss_channel(Feed, _, _) ->
    Feed.

rss_channel_item(Entry, "guid", Text) ->
    Entry#entry{id = Text};
rss_channel_item(Entry, "title", Text) ->
    Entry#entry{title = Text};
rss_channel_item(Entry, "link", Text) ->
    Entry#entry{link = Text};
rss_channel_item(Entry, "content:encoded", Text) ->
    Entry#entry{description = Text};
rss_channel_item(#entry{description = undefined} = Entry, "encoded", Text) ->
    Entry#entry{description = Text};
rss_channel_item(#entry{description = undefined} = Entry, "description", Text) ->
    Entry#entry{description = Text};
rss_channel_item(Entry, _, _) ->
    Entry.


%% ATOM

atom_feed(Feed, "title", Text) ->
    Feed#feed{title = Text};
atom_feed(Feed, "subtitle", Text) ->
    Feed#feed{description = Text};
atom_feed(Feed, _, _) ->
    Feed.


atom_entry(Entry, "id", Text) ->
    Entry#entry{id = Text};
atom_entry(Entry, "title", Text) ->
    Entry#entry{title = Text};
atom_entry(Entry, "content", Text) ->
    Entry#entry{description = Text};
atom_entry(Entry, _, _) ->
    Entry.
