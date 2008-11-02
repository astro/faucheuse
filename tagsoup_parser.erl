%% TODO: charsets
-module(tagsoup_parser).

-export([start_link/1, push/2, stop/1]).


-record(tagsoup_parser, {pid, ref}).
-record(state, {ref, callback, buffer = "", encoding = "utf-8"}).

start_link(CallbackFun) ->
    Ref = make_ref(),
    Pid = spawn_link(
	    fun() ->
		    try init(Ref, CallbackFun)
		    catch
			exit:normal ->
			    normal;
			  exit:Reason ->
			    error_logger:error_msg("tagsoup_parser exiting: ~p",
						   [Reason]),
			    exit(Reason)
		    end
	    end),
    {ok, #tagsoup_parser{pid = Pid,
			 ref = Ref}}.

push(#tagsoup_parser{pid = Pid, ref = Ref}, Chars) ->
    Pid ! {push, Ref, Chars}.

stop(#tagsoup_parser{pid = Pid, ref = Ref}) ->
    Pid ! {stop, Ref, self()},
    receive
	{stopped, Ref, Pid} ->
	    ok
    end.

init(Ref, CallbackFun) ->
    State = #state{ref = Ref, callback = CallbackFun},
    parse_text(State).

pull(State) ->
    {NewState, [C]} = pull(State, 1),
    {NewState, C}.

pull(State, NChars) ->
    pull(State, NChars, []).

pull(State, 0, Result) ->
    {State, lists:reverse(Result)};

pull(#state{ref = Ref, buffer = ""} = State, N, Result) ->
    receive
	{push, Ref, Chars} ->
	    pull(State#state{buffer = Chars},
		 N, Result);
	{stop, Ref, Caller} ->
	    Caller ! {stopped, Ref, self()},
	    exit(normal)
    end;

pull(#state{buffer = [Char | Buffer]} = State, N, Result) ->
    pull(State#state{buffer = Buffer}, N - 1, [Char | Result]).

push_back(#state{buffer = Buffer} = State, Chars) ->
    State#state{buffer = Chars ++ Buffer}.

%% Parsing

parse_text(State) ->
    parse_text(State, "").

parse_text(State, Text) ->
    try pull(State) of
	{NewState, C} ->
	    case C of
		$< ->
		    case Text of
			"" -> ignore;
			_ -> emit(State, {text, lists:reverse(Text)})
		    end,
		    parse_tag(NewState);
		_ ->
		    parse_text(NewState, [C | Text])
	    end
    catch exit:normal ->
	    if
		Text =/= "" ->
		    emit(State, {text, lists:reverse(Text)});
		true ->
		    ok
	    end,
	    exit(normal)
    end.

parse_tag(State) ->
    parse_tag(State, "").

parse_tag(State, Name) ->
    {NewState, C} = pull(State),
    case {Name, C} of
	{"-!", $-} ->
	    parse_comment(NewState);
	%% <![CDATA[
	{"ATADC[!", $\[} ->
	    parse_cdata(NewState);
	{"", $ } ->
	    %% Invalid: unescaped >
	    parse_text(NewState, "> ");
	{"", $\t} ->
	    %% Invalid: unescaped >
	    parse_text(NewState, "> ");
	{"", $\n} ->
	    %% Invalid: unescaped >
	    parse_text(NewState, "> ");
	{"", $\r} ->
	    %% Invalid: unescaped >
	    parse_text(NewState, "> ");
	{"", $?} ->
	    parse_pi(NewState);
	{[_ | _], _} when C == $/; C == $> ->
	    NewState2 = push_back(NewState, [C]),
	    parse_attributes(NewState2, lists:reverse(Name));
	{_, $ } ->
	    parse_attributes(NewState, lists:reverse(Name));
	{_, $\t} ->
	    parse_attributes(NewState, lists:reverse(Name));
	{_, $\n} ->
	    parse_attributes(NewState, lists:reverse(Name));
	{_, $\r} ->
	    parse_attributes(NewState, lists:reverse(Name));
	{_, _} ->
	    parse_tag(NewState, [C | Name])
    end.

parse_attributes(State, TagName) ->
    parse_attributes(State, TagName, []).

parse_attributes(State, TagName, Attributes) ->
    {NewState, C} = pull(State),
    case C of
	$  ->
	    parse_attributes(NewState, TagName, Attributes);
	$\t ->
	    parse_attributes(NewState, TagName, Attributes);
	$\n ->
	    parse_attributes(NewState, TagName, Attributes);
	$\r ->
	    parse_attributes(NewState, TagName, Attributes);
	$/ ->
	    emit(NewState, {start_element,
			    TagName,
			    lists:reverse(Attributes)}),
	    parse_attributes(NewState, [$/ | TagName], []);
	$> ->
	    case TagName of
		[$/ | TagName1] ->
		    emit(NewState, {end_element, TagName1});
		_ ->
		    emit(NewState, {start_element,
				    TagName,
				    lists:reverse(Attributes)})
	    end,
	    parse_text(NewState);
	_ ->
	    {NewState2, AttrName, AttrValue} =
		parse_attribute_name(NewState, [C]),
	    parse_attributes(NewState2, TagName,
			     [{AttrName, AttrValue} | Attributes])
    end.

parse_attribute_name(State, Name) ->
    {NewState, C} = pull(State),
    case C of
	$  ->
	    {NewState, lists:reverse(Name), lists:reverse(Name)};
	$\t ->
	    {NewState, lists:reverse(Name), lists:reverse(Name)};
	$\n ->
	    {NewState, lists:reverse(Name), lists:reverse(Name)};
	$\r ->
	    {NewState, lists:reverse(Name), lists:reverse(Name)};
	$/ ->
	    NewState2 = push_back(NewState, "/"),
	    {NewState2, lists:reverse(Name), lists:reverse(Name)};
	$= ->
	    parse_attribute_value(NewState, lists:reverse(Name));
	_ ->
	    parse_attribute_name(NewState, [C | Name])
    end.

parse_attribute_value(State, Name) ->
    {NewState, C} = pull(State),
    case C of
	$' ->
	    parse_attribute_value(NewState, Name, "'", "");
	$" ->
	    parse_attribute_value(NewState, Name, "\"", "");
	_ ->
	    parse_attribute_value(NewState, Name, none, [C])
    end.

parse_attribute_value(State, Name, none, Value) ->
    {NewState, C} = pull(State),
    case C of
	$  ->
	    {NewState, Name, lists:reverse(Value)};
	$\t ->
	    {NewState, Name, lists:reverse(Value)};
	$\n ->
	    {NewState, Name, lists:reverse(Value)};
	$\r ->
	    {NewState, Name, lists:reverse(Value)};
	$> ->
	    NewState2 = push_back(NewState, ">"),
	    {NewState2, Name, lists:reverse(Value)};
	_ ->
	    parse_attribute_value(NewState, Name, none, [C | Value])
    end;

parse_attribute_value(State, Name, Delim, Value) ->
    {NewState, C} = pull(State),
    case [C] of
	Delim ->
	    {NewState, Name, lists:reverse(Value)};
	_ ->
	    parse_attribute_value(NewState, Name, Delim, [C | Value])
    end.


parse_cdata(State) ->
    parse_cdata(State, "").

%% ]]>
parse_cdata(State, [$>, $\], $\] | R]) ->
    emit(State, {text, lists:reverse(R)}),
    parse_text(State);
parse_cdata(State, R) ->
    {NewState, C} = pull(State),
    parse_cdata(NewState, [C | R]).


parse_comment(State) ->
    parse_comment(State, "").

parse_comment(State, Comment) ->
    {NewState, C} = pull(State),
    case {Comment, C} of
	{[$-, $- | _Content], $>} ->
	    %%emit(NewState, {comment, lists:reverse(Content)}),
	    parse_text(NewState);
	_ ->
	    parse_comment(NewState, [C | Comment])
    end.

parse_pi(State) ->
    parse_pi(State, "").

parse_pi(State, Name) ->
    {NewState, C} = pull(State),
    case C of
	$  ->
	    parse_pi_attributes(NewState, lists:reverse(Name));
	$\t ->
	    parse_pi_attributes(NewState, lists:reverse(Name));
	$\n ->
	    parse_pi_attributes(NewState, lists:reverse(Name));
	$\r ->
	    parse_pi_attributes(NewState, lists:reverse(Name));
	$? ->
	    parse_pi_attributes(NewState, lists:reverse(Name));
	_ ->
	    parse_pi(NewState, [C | Name])
    end.

parse_pi_attributes(State, Name) ->
    parse_pi_attributes(State, Name, []).

parse_pi_attributes(State, PiName, Attributes) ->
    {NewState, C} = pull(State),
    case C of
	$  ->
	    parse_pi_attributes(NewState, PiName, Attributes);
	$\t ->
	    parse_pi_attributes(NewState, PiName, Attributes);
	$\n ->
	    parse_pi_attributes(NewState, PiName, Attributes);
	$\r ->
	    parse_pi_attributes(NewState, PiName, Attributes);
	$? ->
	    parse_pi_attributes(NewState, PiName, Attributes);
	$> ->
	    NewState2 =
		case PiName of
		    "xml" ->
			case lists:keysearch("encoding", 1, Attributes) of
			    {value, {_, Encoding}} ->
				NewState#state{encoding = Encoding};
			    _ ->
				NewState
			end;
		    _ ->
			NewState
		end,
	    emit(NewState2, {pi, PiName, lists:reverse(Attributes)}),
	    parse_text(NewState2);
	_ ->
	    {NewState2, AttrName, AttrValue} =
		parse_attribute_name(NewState, [C]),
	    parse_pi_attributes(NewState2, PiName,
				[{AttrName, AttrValue} | Attributes])
    end.
    
emit(#state{callback = Callback, encoding = Encoding}, Msg) ->
    Callback(convert_message_encoding(Encoding, Msg)).

convert_message_encoding(Encoding, {text, Text}) ->
    {text, entities:replace_all(
	     try_convert_encoding(
	       Encoding, Text))};

convert_message_encoding(Encoding, {start_element, Name, Attributes}) ->
    Attributes2 =
	[{N, entities:replace_all(
	       try_convert_encoding(
		 Encoding, V))} || {N, V} <- Attributes],
    {start_element, Name, Attributes2};

convert_message_encoding(_Encoding, Msg) ->
    Msg.
    
-define(DESIRED_ENCODING, "UTF-8").
-define(FALLBACK_ENCODING, "ISO8859-1").
try_convert_encoding(Encoding, Text) ->
    case iconv:convert(Encoding, ?DESIRED_ENCODING,
		       Text) of
	{ok, Text2} ->
	    Text2;
	error ->
	    {ok, Text2} = iconv:convert(?FALLBACK_ENCODING, ?DESIRED_ENCODING,
					Text),
	    Text2
    end.
