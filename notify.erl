-module(notify).
%%
%% A generic PubSub pattern
%%

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, unsubscribe/0, notify/2, filter_active_urls/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).
-record(subscription, {url, subscriber}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


subscribe(URL) ->	    
    gen_server:call(?SERVER, {subscribe, URL, self()}).
		    
unsubscribe() ->	    
    gen_server:call(?SERVER, {unsubscribe, self()}).

notify(URL, Info) ->
    gen_server:cast(?SERVER, {notify, URL, Info}).

filter_active_urls(URLs) ->
    gen_server:call(?SERVER, {filter_active_urls, URLs}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    mnesia:create_table(subscription,
			[{type, bag},
			 {attributes, record_info(fields, subscription)}]),
    mnesia:add_table_index(subscription, subscriber),
    {ok, #state{}}.

handle_call({subscribe, URL, Subscriber}, _From, State) ->
    updater:add_update(URL),
    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  mnesia:write(#subscription{url = URL,
					     subscriber = Subscriber})
	  end),
    {reply, ok, State};

handle_call({unsubscribe, Subscriber}, _From, State) ->
    {atomic, Unsubscribed} =
	mnesia:transaction(
	  fun() ->
		  mnesia:write_lock_table(subscription),
		  Subscriptions =
		      mnesia:select(subscription,
				    [{#subscription{subscriber = Subscriber,
						    _ = '_'}, [], ['$_']}]),
		  lists:foldl(
		    fun(Subscription, Unsubscribed) ->
			    mnesia:delete_object(Subscription),
			    Unsubscribed + 1
		    end, 0, Subscriptions)
	  end),
    {reply, Unsubscribed, State};

handle_call({filter_active_urls, URLs}, _From, State) ->
    {atomic, URLs2} =
	mnesia:transaction(
	  fun() ->
		  lists:foldl(
		    fun(URL, {Active, Inactive}) ->
			    case mnesia:read({subscription, URL}) of
				[] -> {Active, [URL | Inactive]};
				_ -> {[URL | Active], Inactive}
			    end
		    end, {[], []}, URLs)
	  end),
    {reply, URLs2, State}.

handle_cast({notify, URL, Info}, State) ->
    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  Subscriptions =
		      mnesia:read({subscription, URL}),
		  lists:foreach(
		    fun(#subscription{subscriber = Subscriber}) ->
			    Subscriber ! {notify, URL, Info}
		    end, Subscriptions)
	  end),
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

