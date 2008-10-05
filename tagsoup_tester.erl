-module(tagsoup_tester).

-export([run_timeout/1, run/0, run/1]).


-include_lib("kernel/include/file.hrl").

run_timeout(T) ->
    Pid = spawn_link(fun run/0),
    receive
	after T ->
	      exit(Pid, kill)
	end.

run() ->
    run("tests").

run(Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    lists:foreach(
      fun([$. | _]) ->
	      ignore;
	 (File) ->
	      Filename = Dir ++ "/" ++ File,
	      case file:read_file_info(Filename) of
		  {ok, #file_info{type = directory}} ->
		      run(Filename);
		  {ok, #file_info{type = regular}} ->
		      run_test(Filename);
		  _ ->
		      ignore
	      end
      end, Filenames).

run_test(Filename) ->
    io:format("Running test on ~s~n", [Filename]),
    {ok, Binary} = file:read_file(Filename),
    Content = binary_to_list(Binary),
    {ok, P} = tagsoup_parser:start_link(
		fun({start_element, Name, _} = M) when length(Name) > 20 ->
			io:format("~s: ~p~n", [Filename, M]);
		   ({end_element, Name} = M) when length(Name) > 20 ->
			io:format("~s: ~p~n", [Filename, M]);
		   (_) ->
			ignore
		end),
    tagsoup_parser:push(P, Content),
    tagsoup_parser:stop(P).

