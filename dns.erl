-module(dns).
-export([start/0, start/1, lookup/1]).
-export([run/1, main/2, do_lookup/2]).

start() ->
    start(100). % Default LookupInterval is 100ms

start(LookupInterval) ->
    spawn_link(?MODULE, run, [LookupInterval]).

% Caller will receive {lookup, Name, [Addresses]}
lookup(Name) ->
    dns ! {self(), lookup, Name}.



run(LookupInterval) ->
    dbg:tpl(?MODULE,[{'_',[],[{message,{return_trace}}]}]),
    register(dns, self()),
    % {Name, Inet4 | pending, Inet6 | pending}
    Results = ets:new(dns_results, []),
    % {do_lookup, Caller, Name, Family}
    LookupQueue = timed_queue:create(LookupInterval),

    main(LookupQueue, Results).


main(LookupQueue, Results) ->
    io:format("dns:main(~p, ~p)~n", [LookupQueue, Results]),
    receive

	{Caller, lookup, Name} -> % Triggered by lookup/1
	    io:format("~p lookup ~p~n", [Caller, Name]),
	    case ets:lookup(Results, Name) of
		[] -> % Haven't looked up yet
		    io:format("Adding do_lookup ~p to LookupQueue~n", [Name]),
		    timed_queue:add(LookupQueue, {do_lookup, Caller, Name, inet}),
		    timed_queue:add(LookupQueue, {do_lookup, Caller, Name, inet6});
		[{Name, Inet, Inet6}] -> % Already looked up
		    Caller ! {lookup, Name, Inet ++ Inet6}
	    end;

	{do_lookup, Caller, Name, Family} -> % Triggered by LookupQueue
						% It is impossible to have result already,
						% because that would mean we're delivering delayed
						% although the answer is already known
	    io:format("do_lookup ~p ~p ~p~n", [Caller, Name, Family]),
	    io:format("Results[~p] = ~p~n", [Name, ets:lookup(Results, Name)]),
	    DoSend = case {Family, ets:lookup(Results, Name)} of
			 {inet, []} -> true;
			 {inet, [{Name, nil, _}]} -> true;
			 {inet, _} -> false;
			 {inet6, []} -> true;
			 {inet6, [{Name, _, nil}]} -> true;
			 {inet6, _} -> false
		      end,
	    io:format("DoSend: ~p~n", [DoSend]),
	    if
		DoSend ->
		    {Inet, Inet6} = case Family of
					inet ->
					    case ets:lookup(Results, Name) of
						[] ->
						    {pending, nil};
						[{Name, nil, Inet6_}] ->
						    {pending, Inet6_}
					    end;
					inet6 ->
					    case ets:lookup(Results, Name) of
						[] ->
						    {nil, pending};
						[{Name, Inet_, nil}] ->
						    {Inet_, pending}
					    end
				    end,
		    ets:insert(Results, {Name, Inet, Inet6}),
		    spawn_link(?MODULE, do_lookup, [Name, Family]);
		true ->
		    nil
	    end,

	    % Put back in queue
	    timed_queue:add(LookupQueue, {do_lookup, Caller, Name, Family});

	{lookup_result, Name, Family, Addresses} = A ->
	    % Insert into Results
	    io:format("~p (~p)~n", [A, ets:lookup(Results, Name)]),
	    case Family of
		inet ->
		    Inet = Addresses,
		    [{Name, pending, Inet6}] = ets:lookup(Results, Name);
		inet6 ->
		    Inet6 = Addresses,
		    [{Name, Inet, pending}] = ets:lookup(Results, Name)
	    end,
	    ets:insert(Results, {Name, Inet, Inet6}),

		% Nothing nil or pending anymore?
	    Notify = is_list(Inet) and is_list(Inet6),
	    io:format("Notify: ~p~n", [Notify]),
	    Dns = self(),
	    timed_queue:filter(LookupQueue,
			       fun({do_lookup, Caller, Name_, Family_})
				  when (Name =:= Name_) and (Family =:= Family_) ->
				       if
					   Notify ->
					       Dns ! {notify_requester, Caller, Name, Inet ++ Inet6},
					       false;
					   true ->
					       false
				       end;
				  ({do_lookup, _, _, _}) ->
				       true
			       end),
	    io:format("Filtered~n");

	{notify_requester, Caller, Name, Addresses} ->
	    Caller ! {lookup, Name, Addresses}
    end,
    ?MODULE:main(LookupQueue, Results).

%%
% Perform the actual query
do_lookup(Name, Family) ->
    io:format("dns:do_lookup(~p, ~p)~n", [Name, Family]),
    Addresses = case inet:getaddrs(Name, Family) of
		    {ok, A} ->
			A;
		    {error, _} ->
			[]
		end,
    dns ! {lookup_result, Name, Family, Addresses}.
