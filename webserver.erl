-module(webserver).

-behaviour(supervisor).

%% API
-export([proc_for_port/1, start_link/2, iserve_request/2]).

%% Supervisor callbacks
-export([init/1]).

-include("vendor/iserve/include/iserve.hrl").

%%====================================================================
%% API functions
%%====================================================================
proc_for_port(Port) ->
    list_to_atom(atom_to_list(?MODULE) ++ "-" ++ integer_to_list(Port)).

start_link(Port, Config) ->
    ProcName = proc_for_port(Port),
    supervisor:start_link({local, ProcName}, ?MODULE, [Port, Config]).

iserve_request({c, _Sock, Port, _PeerAddr, _PeerPort, _CbMod, _CbData},
	       #req{uri = {abs_path, Path}} = Req) ->
    ProcName = proc_for_port(Port),
    Handled =
	lists:foldl(
	  fun({Id,Child,_Type,_Modules}, false) ->
		  try worker_path(Id) of
		      ChildPath ->
			  case starts_with(Path, ChildPath) of
			      false ->
				  false;
			      Rest ->
				  worker_request(Child, Rest, Req)
			  end
		  catch _:_ ->
			  false
		  end;
	     (_, R) ->
		  R
	  end, false, supervisor:which_children(ProcName)),
    case Handled of
	false ->
	    {respond, 404, [], "Not found"};
	Res ->
	    Res
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([Port, Config]) ->
    {ok,{{one_for_one,2,1}, [
			     {iserve, {iserve_server, start_link, [Port, ?MODULE]},
			      permanent, 1000, worker, [iserve_server]}
			     | [child_spec(WorkerConf)
				|| WorkerConf <- Config]
			    ]}}.

child_spec(WorkerConf) ->
    Path = element(2, WorkerConf),
    Id = path_worker(Path),
    Module = case element(1, WorkerConf) of
		 xslt -> webserver_xslt;
		 static -> webserver_static
	     end,
    {Id, {Module, start_link, [WorkerConf]},
     permanent, 1000, worker, [Module]}.

%%====================================================================
%% Internal functions
%%====================================================================

path_worker(Path) ->
    list_to_atom("webserver_worker_" ++ Path).

worker_path(Id) ->
    "webserver_worker_" ++ Path = atom_to_list(Id),
    Path.

worker_request(Pid, Path, Req) ->
    try gen_server:call(Pid, Req#req{uri = Path})
    catch exit:E ->
	    error_logger:error_msg("~p ~p: ~p~n", [Pid, Req, E]),
	    {respond, 500, [{"Content-type", "text/plain"}],
	     list_to_binary(io_lib:format("~p", [E]))}
    end.

starts_with([C | S], [C | A]) ->
    starts_with(S, A);
starts_with([_ | _] = S, []) ->
    S;
starts_with([], []) ->
    "";
starts_with(_, _) ->
    false.
