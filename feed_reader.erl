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
    Name1 = string:to_lower(Name),
    State2 = start_element(State#state{stack = [Name1 | Stack]}, Attributes),
    if
	State#state.record =/= State2#state.record ->
	    {noreply, State2#state{current = ""}};
	true ->
	    {noreply, State2}
    end;

handle_cast({end_element, Name}, #state{stack = Stack, current = Current} = State) ->
    State2 = State#state{current = util:string_chomp(Current)},
    case end_element_in_stack(Name, Stack) of
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
    io:format("format: ~p~n", [Format]),
    case Format of
	"atom" -> ok;
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
		entries = [rss_channel_item(Entry, Tag,  Current) | Entries]};

end_element(#state{stack = [_, "channel", _]} = State) ->
    State#state{record = false};


%% Fall-through

end_element(State) ->
    State.


rss_channel(Feed, "title", Text) ->
    Feed#feed{title = Text};
rss_channel(Feed, "link", Text) ->
    Feed#feed{link = Text};
rss_channel(Feed, "description", Text) ->
    Feed#feed{description = Text};
rss_channel(Feed, _, _) ->
    Feed.

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
