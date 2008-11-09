-module(updater).

-behaviour(gen_server).

%% API
-export([start_link/0, add_update/1, tick/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TICK_INTERVAL, 1).
-define(REFRESH_INTERVAL, 600).
-record(state, {}).
%% TODO: save ETag and Last-Modified
-record(update, {url, last}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_update(URL) ->
    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read({update, URL}) of
		      [] ->
			  mnesia:write(#update{url = URL,
					       last = 0});
		      _ -> ignore
		  end
	  end).

tick() ->
    gen_server:call(?SERVER, tick).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    mnesia:create_table(update,
			[{attributes, record_info(fields, update)}]),
    mnesia:add_table_index(update, last),
    {ok, _} = timer:apply_interval(?TICK_INTERVAL, ?MODULE, tick, []),
    {ok, #state{}}.

handle_call(tick, _From, State) ->
    TimeMin = util:current_timestamp() - ?REFRESH_INTERVAL,
    {atomic, Updates} =
	mnesia:transaction(
	  fun() ->
		  mnesia:select(update,
				[{#update{last = '$1',
					  _ = '_'},
				  [{'=<', '$1', {const, TimeMin}}],
				  ['$_']}])
	  end),

    UpdateURLs = [URL || #update{url = URL} <- Updates],
    {Active, Inactive} = notify:filter_active_urls(UpdateURLs),

    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  lists:foreach(
		    fun(#update{url = URL} = Update) ->
			    case {lists:member(URL, Active), lists:member(URL, Inactive)} of
				{true, false} ->
				    mnesia:write(Update#update{last = util:current_timestamp()}),
				    error_logger:info_msg("Launching update for ~p~n", [URL]),
				    update_job:start(URL);
				{false, true} ->
				    mnesia:delete(URL)
			    end
		    end, Updates)
	  end),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

