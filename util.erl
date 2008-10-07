-module(util).

-export([hex_to_int/1]).


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

