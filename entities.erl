-module(entities).

-export([init/0, escape/1, replace_all/1, to_string/1, test/0]).

-record(html_entity, {entity, str}).

-define(MAX_ENTITY_LENGTH, 32).

escape(S) ->
    escape(S, "").

escape([], R) ->
    lists:reverse(R);
escape([$< | S], R) ->
    escape(S, [$;, $t, $l, $& | R]);
escape([$> | S], R) ->
    escape(S, [$;, $t, $g, $& | R]);
escape([$& | S], R) ->
    escape(S, [$;, $p, $m, $a, $& | R]);
escape([$" | S], R) ->
    escape(S, [$;, $t, $o, $u, $q, $& | R]);
escape([$' | S], R) ->
    escape(S, [$;, $s, $o, $p, $a, $& | R]);
escape([C | S], R) ->
    escape(S, [C | R]).

replace_all(S) ->
    replace_all(S, "").

replace_all([], R) ->
    lists:reverse(R);
replace_all([$& | S], R) ->
    case string:chr(S, $;) of
	N when N > 0 andalso N < ?MAX_ENTITY_LENGTH ->
	    Entity = string:substr(S, 1, N - 1),
	    S2 = string:substr(S, N + 1),
	    replace_all(S2, lists:reverse(to_string(Entity)) ++ R);
	_ ->
	    replace_all(S, [$& | R])
    end;
replace_all([C | S], R) ->
    replace_all(S, [C | R]).

%% XML Predefined

to_string("quot") -> "\"";
to_string("amp") -> "&";
to_string("apos") -> "'";
to_string("lt") -> "<";
to_string("gt") -> ">";

%% Numeric

to_string([$#, $x | Hex] = Entity) ->
    case (catch util:hex_to_int(Hex)) of
	{'EXIT', _} ->
	    "&" ++ Entity ++ ";";
	CP ->
	    case (catch codepoint_to_utf8(CP)) of
		{'EXIT', _} ->
		    "&" ++ Entity ++ ";";
		U ->
		    U
	    end
    end;

to_string([$# | Dec] = Entity) ->
    case string:to_integer(Dec) of
	{CP, ""} ->
	    case (catch codepoint_to_utf8(CP)) of
		{'EXIT', _} ->
		    "&" ++ Entity ++ ";";
		U ->
		    U
	    end;
	_ ->
	    "&" ++ Entity ++ ";"
    end;

%% Lookup

to_string(Entity) ->
    case mnesia:dirty_read(html_entity, Entity) of
	[#html_entity{str = Str}] -> Str;
	_ -> "&" ++ Entity ++ ";"
    end.


%% See: http://en.wikipedia.org/wiki/Utf-8#Description
%% TODO: endianess

codepoint_to_utf8(CP) when CP =< 16#7F ->
    [CP];

codepoint_to_utf8(CP) when CP =< 16#7FF ->
    [2#11000000 bor (CP bsr 6),
     2#10000000 bor (CP band 2#111111)];

codepoint_to_utf8(CP) when CP =< 16#FFFF ->
    [2#11100000 bor (CP bsr 12),
     2#10000000 bor ((CP bsr 6) band 2#111111),
     2#10000000 bor (CP band 2#111111)];

codepoint_to_utf8(CP) when CP =< 16#10FFFF ->
    [2#11110000 bor (CP bsr 18),
     2#10000000 bor ((CP bsr 12) band 2#111111),
     2#10000000 bor ((CP bsr 6) band 2#111111),
     2#10000000 bor (CP band 2#111111)].


test() ->
    " " = codepoint_to_utf8(32),
    "Ï" = codepoint_to_utf8(207),
    "€" = codepoint_to_utf8(16#20AC),
    "�" = codepoint_to_utf8(16#FFFD),
    "𐑇" = codepoint_to_utf8(66631),
    "foo & bar" = replace_all("foo &amp; bar"),
    "foo & bar" = replace_all("foo & bar"),
    "foo &unknown; bar" = replace_all("foo &unknown; bar"),
    "lästig" = replace_all("l&auml;stig"),
    ok.


%% The following is in no way complete and just serves the purpose of
%% reading the supplied entity definition DTDs

init() ->
    mnesia:create_table(html_entity,
			[{attributes, record_info(fields, html_entity)}]),
    populate_html_entities_from_file("HTMLlat1.ent"),
    populate_html_entities_from_file("HTMLspecial.ent"),
    populate_html_entities_from_file("HTMLsymbol.ent"),
    ok.

populate_html_entities_from_file(Filename) ->
    case file:read_file(Filename) of
	{ok, Contents} ->
	    N = populate_html_entities_from_dtd(binary_to_list(Contents)),
	    error_logger:info_msg("Loaded ~B entity definitions from ~s~n",
				  [N, Filename]);
	{error, Reason} ->
	    error_logger:error_msg("Cannot load entities from ~s: ~p~n",
				   [Filename, Reason])
    end.

populate_html_entities_from_dtd(DTD) ->
    populate_html_entities_from_dtd(DTD, 0).

populate_html_entities_from_dtd(DTD, N) ->
    case read_entity1(DTD) of
	{Name, Dec, DTD2} ->
	    mnesia:dirty_write(#html_entity{entity = Name,
					    str = codepoint_to_utf8(Dec)}),
	    populate_html_entities_from_dtd(DTD2, N + 1);
	done ->
	    N
    end.

read_entity1([$<, $!, $E, $N, $T, $I, $T, $Y, $  | DTD]) ->
    read_entity2(DTD, "");
read_entity1([_ | DTD]) ->
    read_entity1(DTD);
read_entity1([]) ->
    done.

read_entity2([$  | DTD], "") ->
    read_entity2(DTD, "");
read_entity2([$  | DTD], Entity) ->
    read_entity3(DTD, lists:reverse(Entity));
read_entity2([C | DTD], Entity) ->
    read_entity2(DTD, [C | Entity]).

read_entity3([$  | DTD], Name) ->
    read_entity3(DTD, Name);
read_entity3([$C, $D, $A, $T, $A, $  | DTD], Name) ->
    read_entity4(DTD, Name, "");
read_entity3(DTD, _) ->
    read_entity1(DTD).

read_entity4([$  | DTD], Name, "") ->
    read_entity4(DTD, Name, "");
read_entity4([$", $&, $# | DTD], Name, "") ->
    read_entity4(DTD, Name, "");
read_entity4([$;, $" | DTD], Name, Dec) ->
    {Num, ""} = string:to_integer(lists:reverse(Dec)),
    {Name, Num, DTD};
read_entity4([C | DTD], Name, Dec) ->
    read_entity4(DTD, Name, [C | Dec]).
