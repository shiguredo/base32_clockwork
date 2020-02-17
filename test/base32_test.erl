-module(base32_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32, [encode/2, decode/2]).

rfc4648_test() ->
    Data = <<"foobar">>,
    Encoded = encode(rfc4648, Data),
    Decoded = decode(rfc4648, Encoded),
    ?assertEqual(Encoded, base32_rfc4648:encode(Data)),
    ?assertEqual(Decoded, base32_rfc4648:decode(Encoded)).

crockford_nocheck_test() ->
    Data = <<"foobar">>,
    Encoded = encode(crockford, Data),
    Decoded = decode(crockford, Encoded),
    ?assertEqual(Encoded, base32_crockford:encode(Data)),
    ?assertEqual(Decoded, base32_crockford:decode(Encoded)).

