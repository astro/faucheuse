%% Pipelining HTTP client
%%
%% Any error exit means that something is not ok and all pending
%% requests should be canceled.
%%
%% TODO: https, gzip, deflate
-module(http_connection).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket = none, %% gen_tcp
		addr, port,
		mode = line, %% line | packet
		current_line = "",
		current_packet = "",
		packet_length,
		http_code, http_status, http_headers,
		chunked = false,
		dumb = false,
		zstream = none,
		requests = [], %% [#http_request{}]
		state = status %% status | headers | chunksize | body | chunkfooter
	       }).
-include("http_request.hrl").

%% TODO: implement response timeout
-define(CONNECT_TIMEOUT, 10).

%%====================================================================
%% API
%%====================================================================
start_link(Addr, Port) ->
    case gen_server:start_link(?MODULE, [], []) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {connect, Addr, Port}),
	    {ok, Pid};
	R ->
	    R
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({connect, Addr, Port}, #state{socket = none} = State) ->
    Family = case Addr of
		 {_, _, _, _} -> inet;
		 {_, _, _, _, _, _, _, _} -> inet6
	     end,
    case gen_tcp:connect(Addr, Port,
			 [list, Family, {active, true}],
			 ?CONNECT_TIMEOUT * 1000) of
	{ok, Socket} ->
	    error_logger:info_msg("Connected to ~p:~p = ~p~n", [Addr, Port, Socket]),
	    NewState = State#state{addr = Addr,
				   port = Port,
				   socket = Socket},
	    NewState2 = requests_send_ahead(NewState),
	    {noreply, NewState2};
	{error, Reason} ->
	    %% connection error? signal error to all with same address
	    error_logger:error_msg("Cannot connect to ~p:~p: ~p",
				   [Addr, Port, Reason]),
	    I = self(),
	    F = fun() ->
			Requests =
			    mnesia:select(http_request,
					  [{#http_request{scheme = http,
							  addr = Addr,
							  port = Port,
							  _ = '_'},
					    [], ['$_']}]),
			lists:foreach(
			  fun(#http_request{listener = Listener,
					    worker = Worker} = Request)
			     when Worker =:= none;
				  Worker =:= I ->
				  Listener ! {error, Reason},
				  mnesia:delete_object(Request);
			     (_) -> ignore
			  end, Requests)
		end,
	    {atomic, _} = mnesia:transaction(F),
	    {stop, closed, State#state{socket = none, requests = []}}
    end.

handle_info({tcp, _, Data}, State) ->
    NewState = handle_data(Data, State),
    case NewState#state.socket of
	none ->
	    {stop, normal, NewState};
	_ ->
	    {noreply, NewState}
    end;

handle_info({tcp_closed, _}, #state{dumb = true,
				    socket = Socket} = State) ->
    if
	Socket =/= none ->
	    NewState = one_request_done(State),
	    {stop, normal, NewState};
	true ->
	    {stop, normal, State}
    end;

handle_info({tcp_closed, _}, State) ->
    if
	State#state.mode =:= packet ->
	    error_logger:info_msg("closing, packet_length: ~p", [State#state.packet_length]);
	true ->
	    o
    end,
    {stop, closed, State}.

terminate(Reason, #state{requests = [Request | _]})
  when Reason =/= normal ->
    Request#http_request.listener ! {error, Reason},
    F = fun() ->
		mnesia:delete_object(Request)
	end,
    {atomic, _} = mnesia:transaction(F),
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-define(PIPELINE_SEND_AHEAD, 3).

%% Returns new requests
requests_send_ahead(#state{socket = Socket,
			   addr = Addr,
			   port = Port,
			   requests = Requests} = State) ->
    N = ?PIPELINE_SEND_AHEAD - length(Requests),
    I = self(),
    F = fun() ->
		R = case mnesia:select(http_request,
				       [{#http_request{scheme = http,
						       addr = Addr,
						       port = Port,
						       worker = none,
						       _ = '_'},
					 [], ['$_']}], N, write) of
			{R1, _} -> R1;
			'$end_of_table' -> []
		    end,
		lists:map(
		  fun(R1) ->
			  mnesia:delete_object(R1),
			  R2 = R1#http_request{worker = I},
			  mnesia:write(R2),
			  R2
		  end, R)
	end,
    {atomic, NewRequests} = mnesia:transaction(F),
    lists:foreach(
      fun(#http_request{host = Host,
			path = Path}) ->
	      error_logger:info_msg("Sending request for ~p ~p",
				    [Host, Path]),
	      ok = gen_tcp:send(Socket, format_request(Host, Path))
      end, NewRequests),

    case Requests ++ NewRequests of
	[] ->
	    %% close when none left
	    gen_tcp:close(State#state.socket),
	    State#state{requests = [], socket = none};
	Requests2 ->
	    State#state{requests = Requests2}
    end.

format_request(Host, Path) ->
    io_lib:format("GET ~s HTTP/1.1\r\n" ++
		  "Host: ~s\r\n" ++
		  "Connection: Keep-Alive\r\n" ++
		  "Accept-Encoding: chunked, identity\r\n" ++
		  "User-Agent: Harvester-e/0\r\n" ++
		  "\r\n", [Path, Host]).

request_done(Request) ->
    #http_request{listener = Listener} = Request,
    Listener ! {eof},
    F = fun() ->
		mnesia:delete_object(Request)
	end,
    {atomic, _} = mnesia:transaction(F).

one_request_done(#state{dumb = Dumb,
			requests = [Request | Requests],
			socket = Socket,
			zstream = ZStream} = State) ->
    request_done(Request),
    case ZStream of
	none ->
	    ignore;
	_ ->
	    ok = zlib:inflateEnd(ZStream),
	    ok = zlib:close(ZStream)
    end,
    if
	Dumb ->
	    gen_tcp:close(Socket),
	    State#state{socket = none};
	true ->
	    requests_send_ahead(State#state{requests = Requests})
    end.

%% HTTP Engine

handle_data([$\r | Data], #state{mode = line} = State) ->
    handle_data(Data, State);

handle_data([$\n | Data], #state{mode = line,
				 current_line = Line} = State) ->
    NewState = handle_line(lists:reverse(Line),
			   State#state{current_line = ""}),
    handle_data(Data, NewState);

handle_data([Char | Data], #state{mode = line,
				  current_line = Line} = State) ->
    handle_data(Data, State#state{current_line = [Char | Line]});

handle_data(Data, #state{mode = packet,
			 current_packet = Packet,
			 packet_length = 0} = State) ->
    %% handle_packet/2 must either modify mode or packet_length or we
    %% end up in an endless loop!
    NewState = handle_chunk(Packet,
			    State#state{current_packet = ""}),
    NewState2 = handle_packet_end(NewState),
    handle_data(Data, NewState2);

handle_data([_ | _] = Data, #state{mode = packet,
				   current_packet = Packet,
				   packet_length = Length} = State)
  when length(Data) =< Length or Length < 0 ->
    handle_data([], State#state{current_packet = Packet ++ Data,
				packet_length = Length - length(Data)});

handle_data(Data, #state{mode = packet,
			 current_packet = Packet,
			 packet_length = Length} = State)
  when length(Data) > Length ->
    {Data1, Data2} = lists:split(Length, Data),
    handle_data(Data2, State#state{current_packet = Packet ++ Data1,
				   packet_length = 0});

%% TODO: this inefficient code has been obsoleted by the code above
handle_data([Char | Data], #state{mode = packet,
				  current_packet = Packet,
				  packet_length = Length} = State) ->
    handle_data(Data, State#state{current_packet = [Char | Packet],
				  packet_length = Length - 1});

handle_data([], #state{mode = packet,
		       current_packet = [_ | _] = Packet} = State) ->
    handle_chunk(Packet, State#state{current_packet = ""});

handle_data([], State) ->
     State.


%% state = status

handle_line([$H, $T, $T, $P, $/, $1, $., _, $  | StatusLine],
	    #state{state = status} = State) ->
    {Code, [$  | Status]} = string:to_integer(StatusLine),
    State#state{state = headers,
		http_code = Code,
		http_status = Status,
		http_headers = []};

%% state = headers

handle_line("",
	    #state{state = headers,
		   http_code = Code,
		   http_status = Status,
		   http_headers = Headers,
		   requests = [#http_request{host = Host, path = Path,
					     listener = Listener} | _]} = State) ->

    %% Callback!
    error_logger:info_msg("~p got response for ~p ~p:~n~p",
			  [self(), Host, Path, {Code, Status, Headers}]),
    Listener ! {response, Code, Status, Headers},

    %% Compression?
    NewState =
	case lists:keysearch("content-encoding", 1, Headers) of
	    {value, {_, "deflate"}} ->
		ZStream = zlib:open(),
		ok = zlib:inflateInit(ZStream),
		State#state{zstream = ZStream};
	    _ ->
		State#state{zstream = none}
	end,

    %% Transfer encoding?
    Chunked =
	case lists:keysearch("transfer-encoding", 1, Headers) of
	    {value, {_, [$c, $h, $u, $n, $k, $e, $d | _]}} ->
		true;
	    _ ->
		false
	end,
    Length =
	case lists:keysearch("content-length", 1, Headers) of
	    {value, {_, L}} ->
		{LI, ""} = string:to_integer(L),
		LI;
	    _ ->
		unknown
	end,
    Dumb =
	case lists:keysearch("connection", 1, Headers) of
	    {value, {_, C}} ->
		not(string:to_lower(C) =:= "keep-alive");
	    _ ->
		true
	end,
    if
	(Code >= 100 andalso Code =< 199) orelse
	Code == 204 orelse
	Code == 304 orelse
	Length == 0 ->
	    NewState2 = one_request_done(NewState),
	    NewState2#state{mode = line,
			    state = status,
			    chunked = false,
			    dumb = Dumb};
	Chunked ->
	    NewState#state{mode = line,
			   state = chunksize,
			   chunked = true,
			   dumb = Dumb};
	Length =/= unknown ->
	    NewState#state{mode = packet,
			   packet_length = Length,
			   chunked = false,
			   dumb = Dumb};
	true ->
	    NewState#state{mode = packet,
			   packet_length = -1,
			   chunked = false,
			   dumb = true}
    end;

handle_line(HeaderLine,
	    #state{state = headers,
		   http_headers = Headers} = State) ->
    Colon = string:chr(HeaderLine, $:),
    HeaderName = string:to_lower(string:substr(HeaderLine, 1, Colon - 1)),
    HeaderValue = string:strip(string:substr(HeaderLine, Colon + 1)),
    State#state{http_headers = [{HeaderName, HeaderValue} | Headers]};

%% state = chunksize

handle_line([], #state{state = chunksize} = State) ->
    State;

handle_line(ChunkLine, #state{state = chunksize} = State) ->
    case util:hex_to_int(ChunkLine) of
	0 ->
	    NewState = one_request_done(State),
	    NewState#state{mode = line,
			   state = chunkfooter};
	ChunkSize ->
	    State#state{mode = packet,
			packet_length = ChunkSize}
    end;

handle_line([], #state{state = chunkfooter} = State) ->
    State#state{mode = line,
		state = status,
		chunked = false};
handle_line(_, #state{state = chunkfooter} = State) ->
    State.


handle_chunk(Chunk, #state{requests = [#http_request{listener = Listener} | _]} = State) ->
    case State#state.zstream of
	none ->
	    Listener ! {body, Chunk};
	ZStream ->
	    DecompressedChunk =
		lists:flatten([binary_to_list(D) ||
				  D <- zlib:inflate(ZStream, list_to_binary(Chunk))]),
	    Listener ! {body, DecompressedChunk}
    end,
    State.

handle_packet_end(#state{chunked = true} = State) ->
    State#state{mode = line, state = chunksize};

handle_packet_end(#state{chunked = false} = State) ->
    NewState = one_request_done(State),
    NewState#state{mode = line, state = status}.

