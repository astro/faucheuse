-module(webserver_xslt).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {templates_path, collections, processor}).
-include("iserve.hrl").

%%====================================================================
%% API
%%====================================================================
start_link({xslt, _Path, TemplatesPath, Collections}) ->
    gen_server:start_link(?MODULE, [TemplatesPath, Collections], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([TemplatesPath, Collections]) ->
    {ok, Processor} = templates:start_link(Collections),
    {ok, #state{templates_path = TemplatesPath,
		collections = Collections,
		processor = Processor}}.

handle_call(#req{uri = Uri}, _From,
	    #state{templates_path = TemplatesPath,
		   processor = Processor} = State) ->
    Uri2 = [C
	    || C <- Uri,
	       C =/= $/],
    {ok, Type, Result} = templates:process(Processor, TemplatesPath ++ "/" ++ Uri2),
    {reply, {respond, 200, [{"Content-type", Type}], Result}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
