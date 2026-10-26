-module(base32_crockford_test).

-include_lib("eunit/include/eunit.hrl").


encode_nocheck_test() ->
    ?assertEqual(<<"CR">>, base32_crockford:encode(<<"f">>)),
    ?assertEqual(<<"CSQG">>, base32_crockford:encode(<<"fo">>)),
    ?assertEqual(<<"CSQPY">>, base32_crockford:encode(<<"foo">>)),
    ?assertEqual(<<"CSQPYRG">>, base32_crockford:encode(<<"foob">>)),
    ?assertEqual(<<"CSQPYRK1">>, base32_crockford:encode(<<"fooba">>)),
    ?assertEqual(<<"CSQPYRK1E8">>, base32_crockford:encode(<<"foobar">>)).


encode_check_test() ->
    ?assertEqual(<<"CRW">>, base32_crockford:encode_check(<<"f">>)),
    ?assertEqual(<<"CSQGV">>, base32_crockford:encode_check(<<"fo">>)),
    ?assertEqual(<<"CSQPYY">>, base32_crockford:encode_check(<<"foo">>)),
    ?assertEqual(<<"CSQPYRG8">>, base32_crockford:encode_check(<<"foob">>)),
    ?assertEqual(<<"CSQPYRK1U">>, base32_crockford:encode_check(<<"fooba">>)),
    ?assertEqual(<<"CSQPYRK1E86">>, base32_crockford:encode_check(<<"foobar">>)).


%% encode/1 の integer 経路は V を 5 ビットごとのシンボル列に分解した
%% canonical な表現を返す。桁数境界 (2^5, 2^10, 2^15) の具体値を含める。
encode_integer_test() ->
    ?assertEqual(<<"0">>, base32_crockford:encode(0)),
    ?assertEqual(<<"1">>, base32_crockford:encode(1)),
    ?assertEqual(<<"Z">>, base32_crockford:encode(31)),
    ?assertEqual(<<"10">>, base32_crockford:encode(32)),
    ?assertEqual(<<"1A">>, base32_crockford:encode(42)),
    ?assertEqual(<<"30">>, base32_crockford:encode(96)),
    ?assertEqual(<<"7Z">>, base32_crockford:encode(255)),
    ?assertEqual(<<"100">>, base32_crockford:encode(1024)).


%% integer 以外の入力は spec 違反のため function_clause でクラッシュする。
encode_invalid_input_test() ->
    ?assertError(function_clause, base32_crockford:encode(hello)),
    ?assertError(function_clause, base32_crockford:encode(-1)).


%% encode_check/1 の integer 経路は encode0/2 を共有するため、
%% データ部の桁数が正しい canonical な表現になる。
encode_check_integer_test() ->
    ?assertEqual(<<"1A5">>, base32_crockford:encode_check(42)).


decode_nocheck_test() ->
    ?assertEqual({ok, <<"f">>}, base32_crockford:decode(<<"CR">>)),
    ?assertEqual({ok, <<"fo">>}, base32_crockford:decode(<<"CSQG">>)),
    ?assertEqual({ok, <<"foo">>}, base32_crockford:decode(<<"CSQPY">>)),
    ?assertEqual({ok, <<"foob">>}, base32_crockford:decode(<<"CSQPYRG">>)),
    ?assertEqual({ok, <<"fooba">>}, base32_crockford:decode(<<"CSQPYRK1">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQPYRK1E8">>)).


decode_check_test() ->
    ?assertEqual({ok, <<"f">>}, base32_crockford:decode_check(<<"CRW">>)),
    ?assertEqual({ok, <<"fo">>}, base32_crockford:decode_check(<<"CSQGV">>)),
    ?assertEqual({ok, <<"foo">>}, base32_crockford:decode_check(<<"CSQPYY">>)),
    ?assertEqual({ok, <<"foob">>}, base32_crockford:decode_check(<<"CSQPYRG8">>)),
    ?assertEqual({ok, <<"fooba">>}, base32_crockford:decode_check(<<"CSQPYRK1U">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode_check(<<"CSQPYRK1E86">>)).


decode_hyphen_test() ->
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"-CSQPYRK1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"---CSQPYRK1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQPYRK1E8-">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQPYRK1E8---">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQPYR-K1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQPYR---K1E8">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQ-PYR-K1E-8">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_crockford:decode(<<"CSQ--PYR--K1E--8">>)).


%% fuzzing で検出された function_clause クラッシュの回帰テスト。
%% catch-all 節がないと不正文字入力でプロセスが落ちるため、
%% 有効なシンボル以外の入力はすべて {error, invalid_format} を返す。
decode_invalid_symbol_test() ->
    %% rfc4648 形式のパディング付き文字列は crockford では不正な入力になる
    %% (マクロ引数の解析を避けるため、パディング付きの入力は変数経由で渡す)
    Padded = <<"JBSWY3DPFQQFO33SNRSCC==">>,
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"*">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"~">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"U">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"u">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode(Padded)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"CSQ*PY">>)).


%% fuzzing で検出された badmatch クラッシュの回帰テスト。
%% パディングビットが非ゼロの入力は、正当なシンボルのみで構成されていても
%% 正しいエンコード結果ではないため {error, invalid_format} を返す。
decode_nonzero_padding_test() ->
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"CSQPYR">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode(<<"8">>)).


%% fuzzing で検出された badmatch クラッシュの回帰テスト。
%% チェックシンボルを含まない空入力は無効な入力として {error, invalid_format} を返す。
decode_check_empty_test() ->
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<>>)).


%% データ部の decode がエラーを返す入力は、decode_check も同じエラーを返す。
decode_check_invalid_data_test() ->
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<"*G">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<"91JPRV3F5GG5EVVJDHJ22">>)).


%% チェックサム不一致は {error, invalid_format} を返す。
decode_check_mismatch_test() ->
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<"CRX">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<"CSQPYRK1E8U">>)).


%% ハイフンとチェックシンボルの大文字小文字を区別してはならない。
%% fuzzing で検出された入力 (8gOOj) と完了条件の入力の回帰テスト。
decode_check_hyphen_and_lowercase_test() ->
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<"-">>)),
    ?assertEqual({ok, <<"f">>}, base32_crockford:decode_check(<<"CRW-">>)),
    ?assertEqual({ok, <<"f">>}, base32_crockford:decode_check(<<"CRw">>)),
    ?assertEqual({ok, <<68, 0>>}, base32_crockford:decode_check(<<"8gOOj">>)),
    ?assertEqual({error, invalid_format}, base32_crockford:decode_check(<<"CR", 227>>)).


other1_test() ->
    P = <<1, 221, 62, 98, 254, 21, 78, 215, 43, 109, 45, 36, 57, 116, 102, 157>>,
    E = <<"07EKWRQY2N7DEAVD5MJ3JX36KM">>,
    ?assertEqual(E, base32_crockford:encode(P)),
    ?assertEqual({ok, P}, base32_crockford:decode(E)).


other2_test() ->
    %% https://github.com/aiq/basexx/blob/master/test/crockford_spec.lua
    P = <<"Wow, it really works!">>,
    E = <<"AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44">>,
    ?assertEqual(E, base32_crockford:encode(P)),
    ?assertEqual({ok, P}, base32_crockford:decode(E)).
