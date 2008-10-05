%% TODO: charsets
-module(tagsoup_parser).

-export([start_link/1, push/2, stop/1]).


-record(state, {callback, buffer = ""}).

start_link(CallbackFun) ->
    Pid = spawn_link(
	    fun() ->
		    try init(CallbackFun)
		    catch
			exit:normal ->
			    normal;
			  exit:Reason ->
			    error_logger:error_msg("tagsoup_parser exiting: ~p",
						   [Reason]),
			    exit(Reason)
		    end
	    end),
    {ok, Pid}.

push(Pid, Chars) ->
    Pid ! {push, Chars}.

stop(Pid) ->
    Ref = make_ref(),
    Pid ! {stop, self(), Ref},
    receive
	{stopped, Pid, Ref} ->
	    ok
    end.

init(CallbackFun) ->
    State = #state{callback = CallbackFun},
    parse_text(State).

pull(State) ->
    {NewState, [C]} = pull(State, 1),
    {NewState, C}.

pull(State, NChars) ->
    pull(State, NChars, []).

pull(State, 0, Result) ->
    {State, lists:reverse(Result)};

pull(#state{buffer = ""} = State, N, Result) ->
    receive
	{push, Chars} ->
	    pull(State#state{buffer = Chars},
		 N, Result);
	{stop, Caller, Ref} ->
	    Caller ! {stopped, self(), Ref},
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
    {NewState, C} = pull(State),
    case C of
	$< ->
	    case Text of
		"" -> ignore;
		_ -> (State#state.callback)({text, lists:reverse(Text)})
	    end,
	    parse_tag(NewState);
	_ ->
	    parse_text(NewState, [C | Text])
    end.

parse_tag(State) ->
    parse_tag(State, "").

parse_tag(State, Name) ->
    {NewState, C} = pull(State),
    case {Name, C} of
	{"-!", $-} ->
	    parse_comment(NewState);
	{"", $ } ->
	    %% Invalid: unescaped >
	    parse_text(NewState, "> ");
	{"", $?} ->
	    parse_pi(NewState);
	{[_ | _], _} when C == $/; C == $> ->
	    NewState2 = push_back(NewState, [C]),
	    parse_attributes(NewState2, lists:reverse(Name));
	{_, $ } ->
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
	    parse_attributes(NewState, Attributes);
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
	$? ->
	    parse_pi_attributes(NewState, PiName, Attributes);
	$> ->
	    emit(NewState, {pi, PiName, lists:reverse(Attributes)}),
	    parse_text(NewState);
	_ ->
	    {NewState2, AttrName, AttrValue} =
		parse_attribute_name(NewState, [C]),
	    parse_pi_attributes(NewState2, PiName,
				[{AttrName, AttrValue} | Attributes])
    end.
    

emit(#state{callback = Callback}, Msg) ->
    Callback(Msg).
