-module(resolve).
-export([setup/0, resolve/1]).
-export([do_resolve/3]).


setup() ->
    inet_db:add_ns({141,55,192,51}).

inet_res:gethostbyname("www.c3d2.de").                        
inet_res:gethostbyname("spaceboyz.net").                      
inet_db:set_inet6(true).                
inet_res:gethostbyname("spaceboyz.net").
inet_res:gethostbyname("spaceboyz.net").
inet_db:set_inet6(false).               
inet_res:gethostbyname("www.google.com").

resolve(Name) ->
.

do_resolve(Listener, Name, Inet6) ->
    inet_db:set_inet6(Inet6),
    AF = case Inet6 of
	     false -> inet;
	     true -> inet6
	 end,
    case inet_res:gethostbyname(Name) of
	{ok, {hostent, _, _, AF, _, Addresses}} ->
	    Listener ! {AF, Addresses};
	{error, _} ->
	    []
    end,
    Listener ! {AF, Addresses}.

