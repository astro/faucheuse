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
    code:add_path("vendor/erlxslt"),
    code:add_path("vendor/iserve/ebin"),
    application:start(sasl),
    mnesia:create_schema([node()]),
    mnesia:start(),
    entities:init(),
    storage:init(),
    {ok, ConfigWorkers} = config_workers(),
    {ok,{{one_for_one,2,1}, [
			     {iconv, {iconv, start_link, []},
			      permanent, 1000, worker, [iconv]},
			     {tidy, {tidy, start_link, []},
			      permanent, 1000, worker, [tidy]},
			     {dns_cache, {dns_cache, start_link, []},
			      permanent, 2000, worker, [dns_cache, dns_worker]},
			     {http_client, {http_client, start_link, []},
			      permanent, 4000, worker, [http_client, http_connection]},
			     {updater, {updater, start_link, []},
			      permanent, 1000, worker, [updater]},
			     {notify, {notify, start_link, []},
			      permanent, 1000, worker, [notify]}
			     | ConfigWorkers
			    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

config_workers() ->
    config_workers("faucheuse.cfg").

config_workers(Filename) ->
    case file:consult(Filename) of
	{error, {Line, erl_scan, scan}} ->
	    error_logger:error_msg("Syntax error in ~p, line ~p~n",
				   [Filename, Line]),
	    exit(parse_error);
	{error, Reason} ->
	    error_logger:error_msg("Cannot open ~p: ~p~n",
				   [Filename, Reason]),
	    exit(file_error);
	{ok, [WorkerConfig]} ->
	    {ok, lists:map(fun worker_config2config_workers/1,
			   WorkerConfig)}
    end.

worker_config2config_workers({web, Port, Handlers}) ->
    {webserver, {webserver, start_link, [Port, Handlers]},
     permanent, 1000, supervisor, [webserver]}.
