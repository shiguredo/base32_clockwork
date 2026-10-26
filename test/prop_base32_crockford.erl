-module(prop_base32_crockford).

-export([prop_base32_crockford_encode_integer/1]).

-include_lib("proper/include/proper.hrl").


prop_base32_crockford_encode_integer(doc) ->
    "base32_crockford を encode した整数のシンボル列は元の整数と一致する";
prop_base32_crockford_encode_integer(opts) ->
    [{numtests, 100000}, {on_output, fun proper_output/2}].


%% encode/1 の integer 経路は、V を 5 ビットごとのシンボル列に分解した
%% canonical な表現を返すため、シンボル列を再び整数に戻すと V に一致する。
%% decode/1 はバイト境界で切詰めるため binary レベルの roundtrip は
%% integer 経路では成立しない。そのためシンボル値の連結で検証する。
prop_base32_crockford_encode_integer() ->
    ?FORALL(V,
            range(0, 16#FFFFFFFFFFFFFFFF),
            begin
                Encoded = base32_crockford:encode(V),
                decode_int(Encoded) =:= V
            end).


%% エンコード文字列のシンボルを 5 ビットごとに連結して整数に戻す
decode_int(Encoded) ->
    decode_int(binary_to_list(Encoded), 0).

decode_int([], N) ->
    N;
decode_int([C | Rest], N) ->
    decode_int(Rest, (N bsl 5) bor symbol_value(C)).

%% base32_crockford の symbol/1 は未エクスポートのため、テスト内でシンボルの値を定義する。
%% エンコード出力は大文字のみを生成する。
symbol_value($0) -> 0;
symbol_value($1) -> 1;
symbol_value($2) -> 2;
symbol_value($3) -> 3;
symbol_value($4) -> 4;
symbol_value($5) -> 5;
symbol_value($6) -> 6;
symbol_value($7) -> 7;
symbol_value($8) -> 8;
symbol_value($9) -> 9;
symbol_value($A) -> 10;
symbol_value($B) -> 11;
symbol_value($C) -> 12;
symbol_value($D) -> 13;
symbol_value($E) -> 14;
symbol_value($F) -> 15;
symbol_value($G) -> 16;
symbol_value($H) -> 17;
symbol_value($J) -> 18;
symbol_value($K) -> 19;
symbol_value($M) -> 20;
symbol_value($N) -> 21;
symbol_value($P) -> 22;
symbol_value($Q) -> 23;
symbol_value($R) -> 24;
symbol_value($S) -> 25;
symbol_value($T) -> 26;
symbol_value($V) -> 27;
symbol_value($W) -> 28;
symbol_value($X) -> 29;
symbol_value($Y) -> 30;
symbol_value($Z) -> 31.


proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).
