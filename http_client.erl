-module(http_client).

-behaviour(gen_server).

%% API
-export([start_link/0, request/1, recv/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).
-include("url.hrl").
-include("http_request.hrl").
-record(http_connection, {sap, pid}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% TODO: add references to communication
-define(MAX_REDIRECTS, 3).

request(_, 0) ->
    {error, redirects_exceeded};
request(#url{scheme = Scheme,
	     host = Host,
	     port = Port} = URL, Redirects) ->
    Addrs = dns_cache:lookup(Host),
    Result =
	lists:foldl(
	  fun(Addr, {error, _}) ->
		  gen_server:call(?SERVER,
				  {request, #http_request{scheme = Scheme,
							  addr = Addr,
							  port = Port,
							  host = Host,
							  path = url:get_path_query(URL),
							  listener = self()}}),
		  receive
		      Reply ->
			  Reply
		  end;
	     (_, Reply) ->
		  Reply
	  end, {error, nohosts}, Addrs),
    case Result of
	{response, _, _, Headers} = Response ->
	    case lists:keysearch("location", 1, Headers) of
		{value, {_, Location}} ->
		    recv(fun(_, _) -> ignore end, ignore),
		    NewURL = url:join(URL, Location),
		    error_logger:info_msg("Redirect ~p -> ~p",
					  [url:to_string(URL), url:to_string(NewURL)]),
		    request(NewURL, Redirects - 1);
		_ ->
		    Response
	    end;
	{error, _} = Response ->
	    Response
    end.

request(URL) when is_list(URL) ->
    request(url:parse(URL));
request(#url{} = URL) ->
    request(URL, ?MAX_REDIRECTS).

recv(DataFun, Acc0) ->
    receive
	{body, Data} ->
	    Acc1 = DataFun(Data, Acc0),
	    recv(DataFun, Acc1);
	{eof} -> {ok, Acc0};
	{error, Reason} -> {error, Reason, Acc0}
    end.
	    
		    

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    mnesia:create_table(http_connection,
			[{attributes, record_info(fields, http_connection)}]),
    mnesia:create_table(http_request,
			[{type, bag},
			 {attributes, record_info(fields, http_request)}]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({request, #http_request{scheme = Scheme,
				    addr = Addr,
				    port = Port} = Request},
	    _From, State) ->
    SAP = {Scheme, Addr, Port},
    F = fun() ->
		mnesia:write(Request),
		new_connection_t(SAP)
	end,
    {atomic, Reply} = mnesia:transaction(F),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    F = fun() ->
		[Connection] =
		    mnesia:select(http_connection, [{#http_connection{pid = Pid,
								      _ = '_'},
						     [], ['$_']}]),
		mnesia:delete_object(Connection),
		RequestsLeft =
		    lists:foldl(
		      fun(Request, RequestsLeft1) ->
			      mnesia:delete_object(Request),
			      mnesia:write(Request#http_request{worker = none}),
			      RequestsLeft1 + 1
		      end,
		      0,
		      mnesia:select(http_request, [{#http_request{worker = Pid,
								  _ = '_'},
						    [], ['$_']}])),
		if
		    RequestsLeft > 0 ->
			new_connection_t(Connection#http_connection.sap);
		    true ->
			ok
		end
	end,
    {atomic, _} = mnesia:transaction(F),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

new_connection_t({http, Addr, Port} = SAP) ->
    case mnesia:read({http_connection, SAP}) of
	[] ->
	    case http_connection:start_link(Addr, Port) of
		{ok, Pid} ->
		    mnesia:write(#http_connection{sap = SAP,
						  pid = Pid}),
		    {ok, Pid};
		{error, Reason} ->
		    {error, Reason}
	    end;
	[#http_connection{pid = Pid}] ->
	    {ok, Pid}
    end.
