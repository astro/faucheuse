-module(webserver_static).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {files_path}).
-include("iserve.hrl").

%%====================================================================
%% API
%%====================================================================
start_link({static, _Path, FilesPath}) ->
    gen_server:start_link(?MODULE, [FilesPath], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([FilesPath]) ->
    {ok, #state{files_path = FilesPath}}.

handle_call(#req{uri = Uri}, _From, State)
  when Uri =:= "";
       hd(Uri) =/= $/ ->
    {reply, {301, [{"Location", "/"}], ""}, State};
handle_call(#req{uri = Path}, _From, State) ->
    case string:str(Path, "/.") of
	0 ->
	    io:format("reading ~p~n",[State#state.files_path ++ "/" ++ Path]),
	    case file:read_file(State#state.files_path ++ "/" ++ Path) of
		{ok, Content} ->
		    ContentType = content_type_for_path(Path),
		    {reply, {respond, 200, [{"Content-type", ContentType}],
			     Content}, State};
		{error, Reason} ->
		    {reply, {respond, 500, [], atom_to_list(Reason)}, State}
	    end;
	_ ->
	    {reply, {respond, 400, [], "Bad request"}, State}
    end.

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

content_type_for_path(Path) ->
    content_type_for_path_(lists:reverse(Path)).

content_type_for_path_("ssc." ++ _) -> "text/css";
content_type_for_path_(_) -> "text/plain".
