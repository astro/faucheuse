-module(dns_worker).

%% API
-export([start_link/1]).


start_link(Name) ->
    Caller = self(),
    Pid = spawn_link(fun() ->
			     lookup(Caller, Name)
		     end),
    {ok, Pid}.

lookup(Caller, Name) ->
    Result4 = inet:getaddrs(Name, inet),
    Result6 = inet:getaddrs(Name, inet6),
    Result = case {Result4, Result6} of
		{{ok, A4}, {ok, A6}} ->
		    {ok, A4 ++ A6};
		{{ok, A4}, _} ->
		    {ok, A4};
		{{error, Reason}, _} ->
		    {error, Reason}
	    end,
    gen_server:cast(Caller, {result, Name, Result}).
