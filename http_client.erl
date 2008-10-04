-module(http_client).

-behaviour(gen_server).

%% API
-export([start_link/0, request/1, recv/1]).

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

request(#url{scheme = Scheme,
	     host = Host,
	     port = Port} = URL) ->
    %% TODO: redir, addr fallback
    [Addr | _] = dns_cache:lookup(Host),
    gen_server:call(?SERVER, {request, #http_request{scheme = Scheme,
						     addr = Addr,
						     port = Port,
						     host = Host,
						     path = url:get_path_query(URL),
						     listener = self()}}),
    receive
	{response, _, _, _} = Response ->
	    Response;
	{error, _} = Response ->
	    Response
    end;

request(URL) when is_list(URL) ->
    request(url:parse(URL)).

recv(DataFun) ->
    receive
	{body, Data} ->
	    DataFun(Data),
	    recv(DataFun);
	{eof} -> ok;
	{error, Reason} -> {error, Reason}
    after 30000 -> {error, timeout}
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
	    case (catch http_connection:start_link(Addr, Port)) of
		{ok, Pid} ->
		    mnesia:write(#http_connection{sap = SAP,
						  pid = Pid}),
		    {ok, Pid};
		{'ERROR', Reason} ->
		    {error, Reason}
	    end;
	[#http_connection{pid = Pid}] ->
	    {ok, Pid}
    end.
