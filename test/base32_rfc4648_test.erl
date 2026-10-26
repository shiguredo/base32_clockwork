-module(base32_rfc4648_test).

-include_lib("eunit/include/eunit.hrl").


encode_test() ->
    ?assertEqual(<<"MY======">>, base32_rfc4648:encode(<<"f">>)),
    ?assertEqual(<<"MZXQ====">>, base32_rfc4648:encode(<<"fo">>)),
    ?assertEqual(<<"MZXW6===">>, base32_rfc4648:encode(<<"foo">>)),
    ?assertEqual(<<"MZXW6YQ=">>, base32_rfc4648:encode(<<"foob">>)),
    ?assertEqual(<<"MZXW6YTB">>, base32_rfc4648:encode(<<"fooba">>)),
    ?assertEqual(<<"MZXW6YTBOI======">>, base32_rfc4648:encode(<<"foobar">>)).


decode_test() ->
    ?assertEqual({ok, <<"f">>}, base32_rfc4648:decode(<<"MY======">>)),
    ?assertEqual({ok, <<"fo">>}, base32_rfc4648:decode(<<"MZXQ====">>)),
    ?assertEqual({ok, <<"foo">>}, base32_rfc4648:decode(<<"MZXW6===">>)),
    ?assertEqual({ok, <<"foob">>}, base32_rfc4648:decode(<<"MZXW6YQ=">>)),
    ?assertEqual({ok, <<"fooba">>}, base32_rfc4648:decode(<<"MZXW6YTB">>)),
    ?assertEqual({ok, <<"foobar">>}, base32_rfc4648:decode(<<"MZXW6YTBOI======">>)).


%% 不正文字は catch-all 節で {error, invalid_format} を返す
decode_error_test() ->
    ?assertEqual({error, invalid_format}, base32_rfc4648:decode(<<"MZXW6YTBOI======A">>)).
