-module(prop_base32_rfc4648).

-export([prop_base32_rfc4648_encode_decode/1]).

-include_lib("proper/include/proper.hrl").


prop_base32_rfc4648_encode_decode(doc) ->
    "base32_rfc4648 を encode して decode する";
prop_base32_rfc4648_encode_decode(opts) ->
    [{numtests, 100000}, {on_output, fun proper_output/2}].


%% encode した canonical な出力 (パディング付き) は decode で元の値に戻る。
%% range(0, 100) で空 binary も含める。
prop_base32_rfc4648_encode_decode() ->
    ?FORALL(N,
            range(0, 100),
            ?FORALL(RandomBytes,
                    binary(N),
                    base32_rfc4648:decode(base32_rfc4648:encode(RandomBytes)) =:=
                    {ok, RandomBytes})).


proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).
