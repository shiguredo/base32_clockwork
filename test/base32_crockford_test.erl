-module(base32_crockford_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32_crockford, [encode/1,
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

decode_nocheck_test() ->
    ?assertEqual(<<"f">>, decode(<<"CR">>)),
    ?assertEqual(<<"fo">>, decode(<<"CSQG">>)),
    ?assertEqual(<<"foo">>, decode(<<"CSQPY">>)),
    ?assertEqual(<<"foob">>, decode(<<"CSQPYRG">>)),
    ?assertEqual(<<"fooba">>, decode(<<"CSQPYRK1">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQPYRK1E8">>)).

decode_check_test() ->
    ?assertEqual({ok, <<"f">>}, decode_check(<<"CRW">>)),
    ?assertEqual({ok, <<"fo">>}, decode_check(<<"CSQGV">>)),
    ?assertEqual({ok, <<"foo">>}, decode_check(<<"CSQPYY">>)),
    ?assertEqual({ok, <<"foob">>}, decode_check(<<"CSQPYRG8">>)),
    ?assertEqual({ok, <<"fooba">>}, decode_check(<<"CSQPYRK1U">>)),
    ?assertEqual({ok, <<"foobar">>}, decode_check(<<"CSQPYRK1E86">>)).

decode_hyphen_test() ->
    ?assertEqual(<<"foobar">>, decode(<<"-CSQPYRK1E8">>)),
    ?assertEqual(<<"foobar">>, decode(<<"---CSQPYRK1E8">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQPYRK1E8-">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQPYRK1E8---">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQPYR-K1E8">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQPYR---K1E8">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQ-PYR-K1E-8">>)),
    ?assertEqual(<<"foobar">>, decode(<<"CSQ--PYR--K1E--8">>)).

other1_test() ->
    P = <<1,221,62,98,254,21,78,215,43,109,45,36,57,116,102,157>>,
    E = <<"07EKWRQY2N7DEAVD5MJ3JX36KM">>,
    ?assertEqual(E, encode(P)),
    ?assertEqual(P, decode(E)).

