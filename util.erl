-module(util).

-export([hex_to_int/1, string_chomp/1, split/2,
	 current_timestamp/0, current_timestamp_ms/0]).


hex_to_int(S) ->
    hex_to_int(S, 0).

hex_to_int([C | S], R) when C >= $0 andalso C =< $9 ->
    hex_to_int(S, (R bsl 4) + (C - $0));
hex_to_int([C | S], R) when C >= $a andalso C =< $f ->
    hex_to_int(S, (R bsl 4) + (C - $a + 10));
hex_to_int([C | S], R) when C >= $A andalso C =< $F ->
    hex_to_int(S, (R bsl 4) + (C - $A + 10));
hex_to_int(_, R) ->
    R.


string_chomp(S) ->
    S1 = string_chomp_left(S),
    S2 = lists:reverse(S1),
    S3 = string_chomp_left(S2),
    S4 = lists:reverse(S3),
    S4.

string_chomp_left([$  | S]) ->
    string_chomp_left(S);
string_chomp_left([$\t | S]) ->
    string_chomp_left(S);
string_chomp_left([$\r | S]) ->
    string_chomp_left(S);
string_chomp_left([$\n  | S]) ->
    string_chomp_left(S);
string_chomp_left(S) ->
    S.


current_timestamp() ->
    {MS, S, _} = now(),
    MS * 1000000 + S.

current_timestamp_ms() ->
    {MS, S, SS} = now(),
    (MS * 1000000 + S) * 1000000 + SS.
    

split(N, List) ->
    if
	N > length(List) ->
	    {List, ""};
	N < 0 ->
	    {"", List};
	true ->
	    lists:split(N, List)
    end.
