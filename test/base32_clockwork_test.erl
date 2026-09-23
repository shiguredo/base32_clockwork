-module(base32_clockwork_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32_clockwork, [encode/1, decode/1]).


encode_test() ->
    ?assertEqual(<<>>, encode(<<>>)),
    ?assertEqual(<<"CR">>, encode(<<"f">>)),
    ?assertEqual(<<"CSQG">>, encode(<<"fo">>)),
    ?assertEqual(<<"CSQPY">>, encode(<<"foo">>)),
    ?assertEqual(<<"CSQPYRG">>, encode(<<"foob">>)),
    ?assertEqual(<<"CSQPYRK1">>, encode(<<"fooba">>)),
    ?assertEqual(<<"CSQPYRK1E8">>, encode(<<"foobar">>)),
    ?assertEqual(<<"07EKWRQY2N7DEAVD5MJ3JX36KM">>,
                 encode(<<1, 221, 62, 98, 254, 21, 78, 215, 43, 109, 45, 36, 57, 116, 102, 157>>)),
    ?assertEqual(<<"AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44">>,
                 encode(<<"Wow, it really works!">>)).


decode_test() ->
    ?assertEqual({ok, <<>>}, decode(<<>>)),
    ?assertEqual({ok, <<"f">>}, decode(<<"CR">>)),
    ?assertEqual({ok, <<"fo">>}, decode(<<"CSQG">>)),
    ?assertEqual({ok, <<"foo">>}, decode(<<"CSQPY">>)),
    ?assertEqual({ok, <<"foob">>}, decode(<<"CSQPYRG">>)),
    ?assertEqual({ok, <<"fooba">>}, decode(<<"CSQPYRK1">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYRK1E8">>)),
    ?assertEqual({ok, <<1, 221, 62, 98, 254, 21, 78, 215, 43, 109, 45, 36, 57, 116, 102, 157>>},
                 decode(<<"07EKWRQY2N7DEAVD5MJ3JX36KM">>)),
    ?assertEqual({ok, <<"Wow, it really works!">>},
                 decode(<<"AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44">>)).


decode_padding_test() ->
    %% 仕様の公式例 (15 bit のうち末尾 7 bit がゼロ) はパディングビットを除去してデコードできる
    ?assertEqual({ok, <<"f">>}, decode(<<"CR0">>)),
    %% encode(<<"foo">>) にゼロ埋めシンボルを足した 6 文字 (30 bit) の入力でクラッシュしない
    ?assertEqual({ok, <<"foo">>}, decode(<<"CSQPY0">>)),
    %% encode(<<"fooba">>) にゼロ埋めシンボルを足した 9 文字 (45 bit) の入力でクラッシュしない
    ?assertEqual({ok, <<"fooba">>}, decode(<<"CSQPYRK10">>)),
    %% encode(<<"foobar">>) にゼロ埋めシンボルを足した 11 文字 (55 bit) の入力でクラッシュしない
    ?assertEqual({ok, <<"foobar">>}, decode(<<"CSQPYRK1E80">>)),
    %% C1 は末尾 2 bit が 01、CSQPYR は末尾 6 bit が 011000、CSQPYRK11 は末尾 5 bit が 00001 のため拒否する
    ?assertEqual({error, invalid_format}, decode(<<"C1">>)),
    ?assertEqual({error, invalid_format}, decode(<<"CSQPYR">>)),
    ?assertEqual({error, invalid_format}, decode(<<"CSQPYRK11">>)).


decode_error_test() ->
    ?assertEqual({error, invalid_size}, decode(<<"0">>)),
    ?assertEqual({error, invalid_format}, decode(<<"012*">>)).
