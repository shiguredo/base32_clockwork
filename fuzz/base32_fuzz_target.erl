%% Base32 パーサーを libFuzzer で fuzzing するための fuzz target。
%%
%% base32:decode/2 が返す通常の入力エラーはクラッシュではないため、結果は使わずに
%% 捨てる。全フォーマットの decode 経路に 1 つの入力を通す。デコーダーが例外を
%% 送出した場合だけ、例外を捕捉せず fuzzer へ伝播させる。
%% これにより fuzzer はその入力をクラッシュ入力として保存し、例外の理由と
%% スタックトレースをクラッシュレポートへ残せる。
%%
%% crockford_check の decode_check/1 は大文字小文字とハイフンを区別しないため、
%% 同一の意味を持つ入力 (大文字正規化・ハイフン除去) で結果が変わる場合は矛盾と
%% みなし、例外を fuzzer へ伝播させてクラッシュ入力として保存させる。
%%
%% デコーダーの行の到達は rebar.config の fuzzer 設定で feedback に base32 と
%% 各モジュールを指定し、libFuzzer の探索へ渡している。
-module(base32_fuzz_target).

-behaviour(fuzzer_target).

-export([evaluate/1]).


-spec evaluate(binary()) -> ok.
evaluate(Data) ->
    _ = base32:decode(rfc4648, Data),
    _ = base32:decode(crockford, Data),
    check_crockford_check(Data),
    _ = base32:decode(clockwork, Data),
    ok.


%% decode_check/1 は大文字小文字とハイフンを区別してはならない。
%% 大文字正規化した入力とハイフンを除去した入力で結果が変わる場合は
%% 実装の欠陥であり、例外を送出して fuzzer にクラッシュとして扱わせる。
-spec check_crockford_check(binary()) -> ok.
check_crockford_check(Data) ->
    check_normalized(base32_crockford:decode_check(Data),
                     base32_crockford:decode_check(normalize_upper(Data))),
    check_normalized(base32_crockford:decode_check(Data),
                     base32_crockford:decode_check(hyphenless(Data))),
    ok.


%% 同一の意味を持つ入力での結果が異なる場合は矛盾
check_normalized(Result, Normalized) ->
    case Result =:= Normalized of
        true ->
            ok;
        false ->
            erlang:error({inconsistent_decode_check_result, {Result, Normalized}})
    end.


%% ASCII の a から z のみを大文字化する
-spec normalize_upper(binary()) -> binary().
normalize_upper(Data) ->
    normalize_upper0(Data, <<>>).


normalize_upper0(<<>>, Accu) ->
    Accu;
normalize_upper0(<<C, Next/binary>>, Accu) when C >= $a, C =< $z ->
    normalize_upper0(Next, <<Accu/binary, (C - 32)>>);
normalize_upper0(<<C, Next/binary>>, Accu) ->
    normalize_upper0(Next, <<Accu/binary, C>>).


%% ハイフンを除去する
-spec hyphenless(binary()) -> binary().
hyphenless(Data) ->
    binary:replace(Data, <<"-">>, <<>>, [global]).
