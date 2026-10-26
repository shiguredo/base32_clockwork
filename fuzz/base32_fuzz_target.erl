%% Base32 パーサーを libFuzzer で fuzzing するための fuzz target。
%%
%% base32:decode/2 が返す通常の入力エラーはクラッシュではないため、結果は使わずに
%% 捨てる。全フォーマットの decode 経路に 1 つの入力を通す。デコーダーが例外を
%% 送出した場合だけ、例外を捕捉せず fuzzer へ伝播させる。
%% これにより fuzzer はその入力をクラッシュ入力として保存し、例外の理由と
%% スタックトレースをクラッシュレポートへ残せる。
%%
%% デコーダーの行の到達は rebar.config の fuzzer 設定で feedback に base32 と
%% 各モジュールを指定し、libFuzzer の探索へ渡している。
-module(base32_fuzz_target).

-behaviour(fuzzer_target).

-export([evaluate/1]).


-spec evaluate(binary()) -> ok.
evaluate(Data) ->
    % 戻り値は不要なので捨てる。例外だけを fuzzer に検出させる。
    _ = base32:decode(rfc4648, Data),
    _ = base32:decode(crockford, Data),
    _ = base32:decode(crockford_check, Data),
    _ = base32:decode(clockwork, Data),
    ok.
