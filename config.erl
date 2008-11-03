-module(config).

-behaviour(gen_server).

%% API
-export([start_link/1, all_urls/0, collections/0, collection_urls/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
all_urls() ->
    gen_server:call({global, ?SERVER}, all_urls).

collections() ->
    gen_server:call({global, ?SERVER}, collections).

collection_urls(Collection) ->
    gen_server:call({global, ?SERVER}, {collection_urls, Collection}).

start_link(Filename) ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [Filename], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Filename]) ->
    case file:consult(Filename) of
	{error, {Line, erl_scan, scan}} ->
	    error_logger:error_msg("Syntax error in ~p, line ~p~n",
				   [Filename, Line]),
	    {stop, parse};
	{error, Reason} ->
	    error_logger:error_msg("Cannot open ~p: ~p~n",
				   [Filename, Reason]),
	    {stop, Reason};
	{ok, [Config | _]} ->
	    {ok, Config}
    end.

handle_call(all_urls, _From, Config) ->
    Collections = collections(Config),
    Reply = lists:foldl(fun({_Name, URLs}, R) ->
				URLs ++ R
			end, [], Collections),
    {reply, Reply, Config};

handle_call(collections, _From, Config) ->
    {reply, collections(Config), Config};

handle_call({collection_urls, Collection}, _From, Config) ->
    Collections = collections(Config),
    Collection2 = if
		      is_list(Collection) ->
			  list_to_atom(Collection);
		      is_atom(Collection) ->
			  Collection
		  end,
    Reply = case lists:keysearch(Collection2, 1, Collections) of
		{value, {_, URLs}} -> URLs;
		false -> []
	    end,
    {reply, Reply, Config}.

handle_cast(_Msg, Config) ->
    {noreply, Config}.

handle_info(_Info, Config) ->
    {noreply, Config}.

terminate(_Reason, _Config) ->
    ok.

code_change(_OldVsn, Config, _Extra) ->
    {ok, Config}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

collections(Config) ->
    {value, {_, Collections}} = lists:keysearch(collections, 1, Config),
    Collections.

