-module(notify).
%%
%% A generic PubSub pattern
%%

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, unsubscribe/0, notify/2]).

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
    {reply, Unsubscribed, State}.

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

