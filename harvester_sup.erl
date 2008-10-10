-module(harvester_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    mnesia:start(),
    entities:init(),
    {ok,{{one_for_all,0,1}, [
			     {iconv, {iconv, start_link, []},
			      permanent, 1000, worker, [iconv]},
			     {dns_cache, {dns_cache, start_link, []},
			      permanent, 1000, worker, [dns_cache, dns_worker]},
			     {http_client, {http_client, start_link, []},
			      permanent, 1000, worker, [http_client, http_connection]}
			    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
