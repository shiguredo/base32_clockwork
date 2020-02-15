-module(base32_crockford_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32_crockford, [encode/2]).

encode_nocheck_test() ->
    ?assertEqual(<<"CR">>, encode(<<"f">>, false)),
    ?assertEqual(<<"CSQG">>, encode(<<"fo">>, false)),
    ?assertEqual(<<"CSQPY">>, encode(<<"foo">>, false)),
    ?assertEqual(<<"CSQPYRG">>, encode(<<"foob">>, false)),
    ?assertEqual(<<"CSQPYRK1">>, encode(<<"fooba">>, false)),
    ?assertEqual(<<"CSQPYRK1E8">>, encode(<<"foobar">>, false)).

encode_check_test() ->
    ?assertEqual(<<"CRW">>, encode(<<"f">>, true)),
    ?assertEqual(<<"CSQGV">>, encode(<<"fo">>, true)),
    ?assertEqual(<<"CSQPYY">>, encode(<<"foo">>, true)),
    ?assertEqual(<<"CSQPYRG8">>, encode(<<"foob">>, true)),
    ?assertEqual(<<"CSQPYRK1U">>, encode(<<"fooba">>, true)),
    ?assertEqual(<<"CSQPYRK1E86">>, encode(<<"foobar">>, true)).

