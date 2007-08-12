-module(erunit).
-export([assert/2,
	assertEquals/2, assertEquals/3,
	test/2,
	spec/1,
	run/1,
	fail/1]).

test(Name, F) ->
	{Name, spawn(test_process(Name, F, self()))}.

test_process(Name, F, P) ->
	fun() ->
		try F() of
			_ -> P ! {self(), {ok, Name}}
		catch
			throw:{assertionFailed, AssertionName} ->
				P ! {self(), {fail, {Name, AssertionName}}};
			_:Type ->
				P ! {self(), {error, {Name, catch erlang:error(Type)}}}
		end
	end.

assert(Name, Test) ->
	case Test of
		true -> {ok, Name};
		_ -> fail(Name)
	end.

assertEquals(Expected, Actual) ->
	Message = io_lib:format("Expected \"~p\". Got \"~p\"", [Expected, Actual]),
	assertEquals(Message, Expected, Actual).

assertEquals(Name, A, B) ->
	case A == B of
		true -> {ok, Name};
		false -> fail(Name)
	end.
	
spec(L) when is_list(L) ->
	spec(L, ""),
	ok.

spec([], Output) ->
	io:format("~s~n", [Output]);
spec([{Spec, _Tests}|Rest], Output) ->
	NewOutput = Output ++ io_lib:format("~n- ~s", [Spec]),
	spec(Rest, NewOutput).

run(Test) when is_function(Test, 0) -> run([Test]);
run(L)    when is_list(L)           -> run(L, ok, "").

run([], Answer, Output) ->
	io:format("~n~s", [Output]),
	Answer;
run([{Module, Tests}|Rest], Answer, Output) when is_list(Tests) ->
	case length(Output) > 0 of
		true -> io:format("~n~s", [Output]);
		false -> ignore
	end,
	io:format("~n~p:~n\t", [Module]),
	run(Tests ++ Rest, Answer, "");
run([{_TestName, TestPid}|Rest], Answer, Output) ->
	TestMessage = receive
		{TestPid, Message} -> Message
	end,
	case TestMessage of
		{ok, _} ->
			io:format("."),
			run(Rest, Answer, Output);
		{fail, {Name, FailMessage}} ->
			io:format("F"),
			NextOutput = io_lib:format("-~p failed:~n\t~s~n", [Name, FailMessage]),
			run(Rest, fail, string:concat(Output, NextOutput));
		{error, {Name, Stacktrace}} ->
			io:format("E"),
			{'EXIT', {Type, [Method|_]}} = Stacktrace,
			NextOutput = io_lib:format("-~p had error:~n\terror:~s~n\t\t~p~n", [Name, Type, Method]),
			run(Rest, fail, string:concat(Output, NextOutput))
	end.

fail(Name) -> throw({assertionFailed, Name}).
