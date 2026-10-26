-module(base32_crockford_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32_crockford,
        [encode/1,
         encode_check/1,
         decode/1,
         decode_check/1]).


encode_nocheck_test() ->
    ?assertEqual(<<"CR">>, encode(<<"f">>)),
    ?assertEqual(<<"CSQG">>, encode(<<"fo">>)),
    ?assertEqual(<<"CSQPY">>, encode(<<"foo">>)),
    ?assertEqual(<<"CSQPYRG">>, encode(<<"foob">>)),
    ?assertEqual(<<"CSQPYRK1">>, encode(<<"fooba">>)),
    ?assertEqual(<<"CSQPYRK1E8">>, encode(<<"foobar">>)).


encode_check_test() ->
    ?assertEqual(<<"CRW">>, encode_check(<<"f">>)),
    ?assertEqual(<<"CSQGV">>, encode_check(<<"fo">>)),
    ?assertEqual(<<"CSQPYY">>, encode_check(<<"foo">>)),
    ?assertEqual(<<"CSQPYRG8">>, encode_check(<<"foob">>)),
    ?assertEqual(<<"CSQPYRK1U">>, encode_check(<<"fooba">>)),
    ?assertEqual(<<"CSQPYRK1E86">>, encode_check(<<"foobar">>)).


%% encode/1 の integer 経路は V を 5 ビットごとのシンボル列に分解した
%% canonical な表現を返す。桁数境界 (2^5, 2^10, 2^15) の具体値を含める。
encode_integer_test() ->
    ?assertEqual(<<"0">>, encode(0)),
    ?assertEqual(<<"1">>, encode(1)),
    ?assertEqual(<<"Z">>, encode(31)),
    ?assertEqual(<<"10">>, encode(32)),
    ?assertEqual(<<"1A">>, encode(42)),
    ?assertEqual(<<"30">>, encode(96)),
    ?assertEqual(<<"7Z">>, encode(255)),
    ?assertEqual(<<"100">>, encode(1024)),
    ok.


%% integer 以外の入力は spec 違反のため function_clause でクラッシュする。
encode_invalid_input_test() ->
    ?assertError(function_clause, encode(hello)),
    ?assertError(function_clause, encode(-1)),
    ok.


%% encode_check/1 の integer 経路は encode0/2 を共有するため、
%% データ部の桁数が正しい canonical な表現になる。
encode_check_integer_test() ->
    ?assertEqual(<<"1A5">>, encode_check(42)),
    ok.


decode_nocheck_test() ->
    ?assertEqual({ok, <<"f">>}, decode(<<"CR">>)),
    ?assertEqual({ok, <<"fo">>}, decode(<<"CSQG">>)),
    ?assertEqual({ok, <<"foo">>}, decode(<<"CSQPY">>)),
    ?assertEqual({ok, <<"foob">>}, decode(<<"CSQPYRG">>)),
    ?assertEqual({ok, <<"fooba">>}, decode(<<"CSQPYRK1">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYRK1E8">>)).


decode_check_test() ->
    ?assertEqual({ok, <<"f">>}, decode_check(<<"CRW">>)),
    ?assertEqual({ok, <<"fo">>}, decode_check(<<"CSQGV">>)),
    ?assertEqual({ok, <<"foo">>}, decode_check(<<"CSQPYY">>)),
    ?assertEqual({ok, <<"foob">>}, decode_check(<<"CSQPYRG8">>)),
    ?assertEqual({ok, <<"fooba">>}, decode_check(<<"CSQPYRK1U">>)),
    ?assertEqual({ok, <<"foobar">>}, decode_check(<<"CSQPYRK1E86">>)).


decode_hyphen_test() ->
    ?assertEqual({ok, <<"foobar">>}, decode(<<"-CSQPYRK1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"---CSQPYRK1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYRK1E8-">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYRK1E8---">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYR-K1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYR---K1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQ-PYR-K1E-8">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQ--PYR--K1E--8">>)).


%% fuzzing で検出された function_clause クラッシュの回帰テスト。
%% catch-all 節がないと不正文字入力でプロセスが落ちるため、
%% 有効なシンボル以外の入力はすべて {error, invalid_format} を返す。
decode_invalid_symbol_test() ->
    %% rfc4648 形式のパディング付き文字列は crockford では不正な入力になる
    %% (マクロ引数の解析を避けるため、パディング付きの入力は変数経由で渡す)
    Padded = <<"JBSWY3DPFQQFO33SNRSCC==">>,
    ?assertEqual({error, invalid_format}, decode(<<"*">>)),
    ?assertEqual({error, invalid_format}, decode(<<"~">>)),
    ?assertEqual({error, invalid_format}, decode(<<"U">>)),
    ?assertEqual({error, invalid_format}, decode(<<"u">>)),
    ?assertEqual({error, invalid_format}, decode(Padded)),
    ?assertEqual({error, invalid_format}, decode(<<"CSQ*PY">>)),
    ok.


%% fuzzing で検出された badmatch クラッシュの回帰テスト。
%% パディングビットが非ゼロの入力は、正当なシンボルのみで構成されていても
%% 正しいエンコード結果ではないため {error, invalid_format} を返す。
decode_nonzero_padding_test() ->
    ?assertEqual({error, invalid_format}, decode(<<"CSQPYR">>)),
    ?assertEqual({error, invalid_format}, decode(<<"8">>)),
    ok.


%% fuzzing で検出された badmatch クラッシュの回帰テスト。
%% チェックシンボルを含まない空入力は無効な入力として {error, invalid_format} を返す。
decode_check_empty_test() ->
    ?assertEqual({error, invalid_format}, decode_check(<<>>)),
    ok.


%% データ部の decode がエラーを返す入力は、decode_check も同じエラーを返す。
decode_check_invalid_data_test() ->
    ?assertEqual({error, invalid_format}, decode_check(<<"*G">>)),
    ?assertEqual({error, invalid_format}, decode_check(<<"91JPRV3F5GG5EVVJDHJ22">>)),
    ok.


%% チェックサム不一致は {error, invalid_format} を返す。
decode_check_mismatch_test() ->
    ?assertEqual({error, invalid_format}, decode_check(<<"CRX">>)),
    ?assertEqual({error, invalid_format}, decode_check(<<"CSQPYRK1E8U">>)),
    ok.


%% ハイフンとチェックシンボルの大文字小文字を区別してはならない。
%% fuzzing で検出された入力 (8gOOj) と完了条件の入力の回帰テスト。
decode_check_hyphen_and_lowercase_test() ->
    ?assertEqual({error, invalid_format}, decode_check(<<"-">>)),
    ?assertEqual({ok, <<"f">>}, decode_check(<<"CRW-">>)),
    ?assertEqual({ok, <<"f">>}, decode_check(<<"CRw">>)),
    ?assertEqual({ok, <<68, 0>>}, decode_check(<<"8gOOj">>)),
    ?assertEqual({error, invalid_format}, decode_check(<<"CR", 227>>)),
    ok.


other1_test() ->
    P = <<1, 221, 62, 98, 254, 21, 78, 215, 43, 109, 45, 36, 57, 116, 102, 157>>,
    E = <<"07EKWRQY2N7DEAVD5MJ3JX36KM">>,
    ?assertEqual(E, encode(P)),
    ?assertEqual({ok, P}, decode(E)),
    ok.


other2_test() ->
    %% https://github.com/aiq/basexx/blob/master/test/crockford_spec.lua
    P = <<"Wow, it really works!">>,
    E = <<"AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44">>,
    ?assertEqual(E, encode(P)),
    ?assertEqual({ok, P}, decode(E)),
    ok.
