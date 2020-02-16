-module(base32_rfc4648_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32_rfc4648, [encode/1, decode/1]).

encode_test() ->
    ?assertEqual(<<"MY======">>, encode(<<"f">>)),
    ?assertEqual(<<"MZXQ====">>, encode(<<"fo">>)),
    ?assertEqual(<<"MZXW6===">>, encode(<<"foo">>)),
    ?assertEqual(<<"MZXW6YQ=">>, encode(<<"foob">>)),
    ?assertEqual(<<"MZXW6YTB">>, encode(<<"fooba">>)),
    ?assertEqual(<<"MZXW6YTBOI======">>, encode(<<"foobar">>)).

decode_test() ->
    ?assertEqual(<<"f">>, decode(<<"MY======">>)),
    ?assertEqual(<<"fo">>, decode(<<"MZXQ====">>)),
    ?assertEqual(<<"foo">>, decode(<<"MZXW6===">>)),
    ?assertEqual(<<"foob">>, decode(<<"MZXW6YQ=">>)),
    ?assertEqual(<<"fooba">>, decode(<<"MZXW6YTB">>)),
    ?assertEqual(<<"foobar">>, decode(<<"MZXW6YTBOI======">>)).

