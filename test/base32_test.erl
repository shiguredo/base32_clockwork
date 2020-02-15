-module(base32_test).

-include_lib("eunit/include/eunit.hrl").

-import(base32, [encode_to_string/2, decode_to_string/2]).

rfc4648_encode_test() ->
    Format = rfc4648,
    ?assertEqual({ok, "MY======"}, encode_to_string(Format, "f")),
    ?assertEqual({ok, "MZXQ===="}, encode_to_string(Format, "fo")),
    ?assertEqual({ok, "MZXW6==="}, encode_to_string(Format, "foo")),
    ?assertEqual({ok, "MZXW6YQ="}, encode_to_string(Format, "foob")),
    ?assertEqual({ok, "MZXW6YTB"}, encode_to_string(Format, "fooba")),
    ?assertEqual({ok, "MZXW6YTBOI======"}, encode_to_string(Format, "foobar")).

rfc4648_decode_test() ->
    Format = rfc4648,
    ?assertEqual({ok, "f"}, decode_to_string(Format, "MY======")),
    ?assertEqual({ok, "fo"}, decode_to_string(Format, "MZXQ====")),
    ?assertEqual({ok, "foo"}, decode_to_string(Format, "MZXW6===")),
    ?assertEqual({ok, "foob"}, decode_to_string(Format, "MZXW6YQ=")),
    ?assertEqual({ok, "fooba"}, decode_to_string(Format, "MZXW6YTB")),
    ?assertEqual({ok, "foobar"}, decode_to_string(Format, "MZXW6YTBOI======")).

