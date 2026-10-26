-module(base32_test).

-include_lib("eunit/include/eunit.hrl").


rfc4648_test() ->
    Data = <<"foobar">>,
    Encoded = base32:encode(rfc4648, Data),
    Decoded = base32:decode(rfc4648, Encoded),
    ?assertEqual(Encoded, base32_rfc4648:encode(Data)),
    ?assertEqual(Decoded, base32_rfc4648:decode(Encoded)).


crockford_nocheck_test() ->
    Data = <<"foobar">>,
    Encoded = base32:encode(crockford, Data),
    {ok, Decoded} = base32:decode(crockford, Encoded),
    ?assertEqual(Encoded, base32_crockford:encode(Data)),
    ?assertEqual({ok, Decoded}, base32_crockford:decode(Encoded)).


crockford_check_test() ->
    Data = <<"foobar">>,
    Encoded = base32:encode(crockford_check, Data),
    {ok, Decoded} = base32:decode(crockford_check, Encoded),
    ?assertEqual(Encoded, base32_crockford:encode_check(Data)),
    ?assertEqual({ok, Decoded}, base32_crockford:decode_check(Encoded)).


clockwork_test() ->
    Data = <<"foobar">>,
    Encoded = base32:encode(clockwork, Data),
    {ok, Decoded} = base32:decode(clockwork, Encoded),
    ?assertEqual(Encoded, base32_clockwork:encode(Data)),
    ?assertEqual({ok, Decoded}, base32_clockwork:decode(Encoded)).
