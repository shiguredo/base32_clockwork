-module(prop_base32_clockwork).

-export([prop_base32_clockwork_encode_decode/1,
         prop_base32_clockwork_case_insensitive_and_alias/1]).

-include_lib("proper/include/proper.hrl").


prop_base32_clockwork_encode_decode(doc) ->
    "base32_clockwork を encode して decode する";
prop_base32_clockwork_encode_decode(opts) ->
    [{numtests, 100000}, {on_output, fun proper_output/2}].


%% encode した canonical な出力は decode で元の値に戻る。
%% range(0, 100) で空 binary も含める。
prop_base32_clockwork_encode_decode() ->
    ?FORALL(N,
            range(0, 100),
            ?FORALL(RandomBytes,
                    binary(N),
                    base32_clockwork:decode(base32_clockwork:encode(RandomBytes)) =:=
                    {ok, RandomBytes})).


prop_base32_clockwork_case_insensitive_and_alias(doc) ->
    "base32_clockwork の encode 結果を小文字化・エイリアス化しても decode できる";
prop_base32_clockwork_case_insensitive_and_alias(opts) ->
    [{numtests, 100000}, {on_output, fun proper_output/2}].


%% decode は大文字小文字を区別せず、O を 0、I と L を 1 のエイリアスとして受理する。
%% canonical な encode 結果を 4 種類の変換で書き換えても decode 結果は変わらない。
prop_base32_clockwork_case_insensitive_and_alias() ->
    ?FORALL(Transform,
            oneof([lowercase, alias_o, alias_i, alias_l]),
            ?FORALL(N,
                    range(0, 100),
                    ?FORALL(RandomBytes,
                            binary(N),
                            begin
                                Encoded = base32_clockwork:encode(RandomBytes),
                                Transformed = replace_symbols(Transform, Encoded),
                                base32_clockwork:decode(Transformed) =:= {ok, RandomBytes}
                            end))).


%% 変換の種類に応じてエンコード出力の各シンボルを書き換える。
%% lowercase は A から Z を小文字化し、alias_o は 0 を O に、alias_i と alias_l は 1 を I と L に置換する
replace_symbols(Type, Encoded) ->
    list_to_binary([ to_symbol(Type, C) || <<C>> <:= Encoded ]).


%% lowercase は A から Z を小文字化し、alias_o は 0 を O に、
%% alias_i と alias_l は 1 を I と L に置換する (その他のシンボルはそのまま)
to_symbol(lowercase, C) when C >= $A, C =< $Z ->
    C + 32;
to_symbol(alias_o, $0) ->
    $O;
to_symbol(alias_i, $1) ->
    $I;
to_symbol(alias_l, $1) ->
    $L;
to_symbol(_, C) ->
    C.


proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).
