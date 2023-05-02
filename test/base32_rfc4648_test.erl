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
    ?assertEqual({ok, <<"f">>}, decode(<<"MY======">>)),
    ?assertEqual({ok, <<"fo">>}, decode(<<"MZXQ====">>)),
    ?assertEqual({ok, <<"foo">>}, decode(<<"MZXW6===">>)),
    ?assertEqual({ok, <<"foob">>}, decode(<<"MZXW6YQ=">>)),
    ?assertEqual({ok, <<"fooba">>}, decode(<<"MZXW6YTB">>)),
    ?assertEqual({ok, <<"foobar">>}, decode(<<"MZXW6YTBOI======">>)).


decode_error_test() ->
    ?assertEqual({error, invalid_format}, decode(<<"MZXW6YTBOI======A">>)).
