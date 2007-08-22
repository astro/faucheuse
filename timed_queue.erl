-module(timed_queue).
-export([create/1, add/2, emit_next/1, filter/2, test/0]).
-export([main/3, test/2]). % Internal use only

create(Interval) ->
    spawn_link(?MODULE, main, [[], Interval, 0]).

add(Pid, E) ->
    Pid ! {self(), add, E}.

%%
% Tell the timed_queue process to take the front element
% from the queue and sent it to the process which added
% added it to the queue.
emit_next(Pid) ->
    Pid ! {self(), emit_next}.

filter(Pid, Filter) ->
    Pid ! {self(), filter, Filter}.

main(Queue, Interval, Last) ->
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
		{_, emit_next} ->
		    {Queue2, Last2} = shift_next(Queue, Last),
		    ?MODULE:main(Queue2, Interval, Last2);
		{_, filter, Filter} ->
		    Queue2 = lists:filter(fun({_, E}) ->
						  Filter(E)
					  end, Queue),
		    ?MODULE:main(Queue2, Interval, Last)
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
	    io:format("<~p> received ~p ~p~n", [now_(), I2, N])
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
