-module(tidy).

-behaviour(gen_server).

%% API
-export([start_link/0, tidy/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% -> {ok, Output} | error
tidy(Input) ->
    gen_server:call(?SERVER, {tidy, Input}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    Port = open_port({spawn, "./tidy"}, [{packet, 4}]),
    {ok, #state{port = Port}}.

handle_call({tidy, Input}, _From, #state{port = Port} = State) ->
    port_command(Port, Input),
    TS = util:current_timestamp_ms(),
    Reply = receive
		{Port, {data, [0 | Result]}} ->
		    Result2 = strip_body(Result),
		    {ok, Result2};
		{Port, {data, [1 | _]}} -> error
	    end,
    %%io:format("Tidy run ~p~n",[util:current_timestamp_ms() - TS]),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-define(HTML1, "<body>").
-define(HTML2, "</body>").
strip_body(S) ->
    S2 = case string:str(S, ?HTML1) of
	     0 -> S;
	     N1 -> string:substr(S, N1 + length(?HTML1))
	 end,
    S3 = case string:rstr(S2, ?HTML2) of
	     0 -> S2;
	     N2 -> string:substr(S2, 1, N2 - 1)
	 end,
    S4 = string:strip(S3, both, $\n),
    S4.
