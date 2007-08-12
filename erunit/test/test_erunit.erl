-module(test_erunit).

-behaviour(erunit_test).
-export([tests/0]).

-import(erunit, [assert/2, assertEquals/3, test/2, fail/1]).
-define(ASSERTION_SHOULD_FAIL(AssertionName, Test),
	failed_as_ok(test(AssertionName, Test), self())
).

failed_as_ok({AssertionName, TestPid}, P) ->
	TestMessage = receive
		{TestPid, Message} -> Message
	end,
	TestAnswer = case TestMessage of
		{fail, {Name, _AssertionName}} -> {ok, Name};
		Val -> {fail, {AssertionName, {"Assertion should have failed", Val}}}
	end,
	{AssertionName, spawn(fun() -> P ! {self(), TestAnswer} end)}.

tests() ->
	[
		test("Should pass with assert true",
			fun() ->
				assert("assert true", true)
			end
		),
		?ASSERTION_SHOULD_FAIL("Should fail with assert false",
			fun() ->
				assert("assert false", false)
			end
		),
		test("Should pass with assertEquals",
			fun() ->
				assertEquals("assert 1 == 1", 1, 1)
			end
		),
		?ASSERTION_SHOULD_FAIL("Should fail with assertEquals",
			fun() ->
				assertEquals("assert 1 != 2", 1, 2)
			end
		),
		?ASSERTION_SHOULD_FAIL("Should fail with fail",
			fun() ->
				fail("Calling fail")
			end
		)
	].