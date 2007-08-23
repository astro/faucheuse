-module(dns).
-export([start/0, start/1, lookup/1, test/0]).
-export([run/1, main/2, do_lookup/2]).

start() ->
    start(100). % Default LookupInterval is 100ms

start(LookupInterval) ->
    Pid = spawn_link(?MODULE, run, [LookupInterval]),
    case (catch register(dns, Pid)) of
	{'EXIT', _} ->
	    exit(Pid, kill),
	    cannot_register;
	_ ->
	    ok
    end.

% Caller will receive {lookup, Name, [Addresses]}
lookup(Name) ->
    dns ! {self(), lookup, Name}.


run(LookupInterval) ->
    dbg:tpl(?MODULE,[{'_',[],[{message,{return_trace}}]}]),
    % {Name, Inet4 | pending, Inet6 | pending}
    Results = ets:new(dns_results, []),
    % {do_lookup, Caller, Name, Family}
    LookupQueue = timed_queue:create(LookupInterval),

    main(LookupQueue, Results).


main(LookupQueue, Results) ->
    receive

	{Caller, lookup, Name} -> % Triggered by lookup/1
	    case ets:lookup(Results, Name) of
		[] -> % Haven't looked up yet
		    timed_queue:add(LookupQueue, {do_lookup, Caller, Name, inet}),
		    timed_queue:add(LookupQueue, {do_lookup, Caller, Name, inet6});
		[{Name, Inet, Inet6}] -> % Already looked up
		    Caller ! {lookup, Name, Inet ++ Inet6}
	    end;

	{do_lookup, Caller, Name, Family} = T -> % Triggered by LookupQueue
						% It is impossible to have result already,
						% because that would mean we're delivering delayed
						% although the answer is already known
	    DoSend = case {Family, ets:lookup(Results, Name)} of
			 {inet, []} -> true;
			 {inet, [{Name, nil, _}]} -> true;
			 {inet, _} -> false;
			 {inet6, []} -> true;
			 {inet6, [{Name, _, nil}]} -> true;
			 {inet6, _} -> false
		     end,
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
		
		% We seem to be already looking up that, reappend
		true ->
		    timed_queue:add(LookupQueue, T)
		    %timed_queue:emit_next(LookupQueue)
	    end,

	    % Put back in queue
	    timed_queue:add(LookupQueue, {do_lookup, Caller, Name, Family});

	{lookup_result, Name, Family, Addresses} ->
	    % Insert into Results
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
			       end);

	{notify_requester, Caller, Name, Addresses} ->
	    io:format("Notifying ~p for ~p: ~p~n", [Caller, Name, Addresses]),
	    Caller ! {lookup, Name, Addresses}
    end,
    ?MODULE:main(LookupQueue, Results).

%%
% Perform the actual query
do_lookup(Name, Family) ->
    io:format("Resolving ~p (~p)~n", [Name, Family]),
    Addresses = case inet:getaddrs(Name, Family) of
		    {ok, A} ->
			A;
		    {error, _} ->
			[]
		end,
    io:format("Resolved ~p (~p): ~p~n", [Name, Family, Addresses]),
    dns ! {lookup_result, Name, Family, Addresses}.


test() ->
    dbg:tpl(?MODULE,[{'_',[],[{message,{return_trace}}]}]),
    case whereis(dns) of
	undefined ->
	    dns:start();
	_ ->
	    already_started
    end,
    
    dns:lookup("spaceboyz.net"),
    dns:lookup("www.google.com"),
    
    receive
	{lookup, "spaceboyz.net", A1} ->
	    io:format("spaceboyz.net is at ~p~n", [A1])
    end,
    receive
	{lookup, "www.google.com", A2} ->
	    io:format("google is at ~p~n", [A2])
    end.

	    
