-module(timed_queue).
-export([create/1, add/2, test/0]).
-export([main/3, test/2]). % Internal use only

create(Interval) ->
    spawn_link(?MODULE, main, [[], Interval, 0]).

add(Pid, E) ->
    Pid ! {self(), add, E}.

main(Queue, Interval, Last) ->
    process_flag(trap_exit, true),
    case Queue of
	[] ->
	    Next = infinity;
	_ ->
	    Next = Last + Interval - now_()
    end,

    if
	Next < 0 ->
	    {Queue2, Last2} = shift_next(Queue, Last),
	    ?MODULE:main(Queue2, Interval, Last2);
	true ->
	    receive
		{Pid, add, E} ->
		    Queue2 = Queue ++ [{Pid, E}],
		    ?MODULE:main(Queue2, Interval, Last);
		{'EXIT', From, Reason} ->
		    io:format("timed_queue: Inflictor (~p) died with reason ~p, exiting~n", [From, Reason]),
		    throw(inflictor_died);
		Garbage ->
		    io:format("timed_queue received garbage: ~p~n", [Garbage])
	    after Next ->
		    {Queue2, Last2} = shift_next(Queue, Last),
		    ?MODULE:main(Queue2, Interval, Last2)
	    end
    end.

shift_next([], Last) ->
    {[], Last};
shift_next([{Pid, E} | Queue], _) ->
    Pid ! E,
    {Queue, now_()}.

% Returns current time in ms
now_() ->
    {M, S, U} = now(),
    (((M * 1000000) + S) * 1000) + trunc(U / 1000).


test() ->
    TQ = create(500),
    test(TQ, 0).

test(TQ, I) ->
    receive
	{elem, I2, N} ->
	    io:format("<~p> test received ~p ~p~n", [now_(), I2, N]);
	{'EXIT', _, _} ->
	    io:format("TQ died~n"),
	    throw(tq_died)
    after 100 ->
	    case random:uniform(30) of
		1 ->
		    test_add(TQ, I, random:uniform(10));
		_ ->
		    nil
	    end
    end,
    ?MODULE:test(TQ, I + 1).

test_add(_, _, 0) ->
    nil;
test_add(TQ, I, N) ->
    io:format("<~p> sending ~p ~p~n", [now_(), I, N]),
    add(TQ, {elem, I, N}),
    test_add(TQ, I, N - 1).
