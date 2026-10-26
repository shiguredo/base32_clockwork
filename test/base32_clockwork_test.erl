-module(base32_clockwork_test).

-include_lib("eunit/include/eunit.hrl").


encode_test() ->
    ?assertEqual(<<>>, base32_clockwork:encode(<<>>)),
    ?assertEqual(<<"CR">>, base32_clockwork:encode(<<"f">>)),
    ?assertEqual(<<"CSQG">>, base32_clockwork:encode(<<"fo">>)),
    ?assertEqual(<<"CSQPY">>, base32_clockwork:encode(<<"foo">>)),
    ?assertEqual(<<"CSQPYRG">>, base32_clockwork:encode(<<"foob">>)),
    ?assertEqual(<<"CSQPYRK1">>, base32_clockwork:encode(<<"fooba">>)),
    ?assertEqual(<<"CSQPYRK1E8">>, base32_clockwork:encode(<<"foobar">>)),
    ?assertEqual(<<"07EKWRQY2N7DEAVD5MJ3JX36KM">>,
                 base32_clockwork:encode(<<1, 221, 62, 98, 254, 21, 78, 215, 43, 109, 45, 36,
                                           57, 116, 102, 157>>)),
    ?assertEqual(<<"AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44">>,
                 base32_clockwork:encode(<<"Wow, it really works!">>)).


decode_test() ->
    ?assertEqual({ok, <<>>}, base32_clockwork:decode(<<>>)),
    ?assertEqual({ok, <<"f">>}, base32_clockwork:decode(<<"CR">>)),
    ?assertEqual({ok, <<"fo">>}, base32_clockwork:decode(<<"CSQG">>)),
    ?assertEqual({ok, <<"foo">>}, base32_clockwork:decode(<<"CSQPY">>)),
    ?assertEqual({ok, <<"foob">>}, base32_clockwork:decode(<<"CSQPYRG">>)),
    ?assertEqual({ok, <<"fooba">>}, base32_clockwork:decode(<<"CSQPYRK1">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_clockwork:decode(<<"CSQPYRK1E8">>)),
    ?assertEqual({ok, <<1, 221, 62, 98, 254, 21, 78, 215, 43, 109, 45, 36, 57, 116, 102, 157>>},
                 base32_clockwork:decode(<<"07EKWRQY2N7DEAVD5MJ3JX36KM">>)),
    ?assertEqual({ok, <<"Wow, it really works!">>},
                 base32_clockwork:decode(<<"AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44">>)).


decode_padding_test() ->
    %% 仕様の公式例 (15 bit のうち末尾 7 bit がゼロ) はパディングビットを除去してデコードできる
    ?assertEqual({ok, <<"f">>}, base32_clockwork:decode(<<"CR0">>)),
    %% encode(<<"foo">>) にゼロ埋めシンボルを足した 6 文字 (30 bit) の入力でクラッシュしない
    ?assertEqual({ok, <<"foo">>}, base32_clockwork:decode(<<"CSQPY0">>)),
    %% encode(<<"fooba">>) にゼロ埋めシンボルを足した 9 文字 (45 bit) の入力でクラッシュしない
    ?assertEqual({ok, <<"fooba">>}, base32_clockwork:decode(<<"CSQPYRK10">>)),
    %% encode(<<"foobar">>) にゼロ埋めシンボルを足した 11 文字 (55 bit) の入力でクラッシュしない
    ?assertEqual({ok, <<"foobar">>}, base32_clockwork:decode(<<"CSQPYRK1E80">>)),
    %% C1 は末尾 2 bit が 01、CSQPYR は末尾 6 bit が 011000、CSQPYRK11 は末尾 5 bit が 00001 のため拒否する
    ?assertEqual({error, invalid_format}, base32_clockwork:decode(<<"C1">>)),
    ?assertEqual({error, invalid_format}, base32_clockwork:decode(<<"CSQPYR">>)),
    ?assertEqual({error, invalid_format}, base32_clockwork:decode(<<"CSQPYRK11">>)).


decode_error_test() ->
    ?assertEqual({error, invalid_format}, base32_clockwork:decode(<<"0">>)),
    ?assertEqual({error, invalid_format}, base32_clockwork:decode(<<"012*">>)).
