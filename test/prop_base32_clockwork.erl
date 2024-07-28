-module(prop_base32_clockwork).

-export([prop_base32_clockwork_encode_decode/1]).

-include_lib("proper/include/proper.hrl").


prop_base32_clockwork_encode_decode(doc) ->
    "base32_clockwork を encode して decode する";
prop_base32_clockwork_encode_decode(opts) ->
    [{numtests, 100000}, {on_output, fun proper_output/2}].


prop_base32_clockwork_encode_decode() ->
    ?FORALL(N,
            range(1, 100),
            ?FORALL(RandomBytes,
                    binary(N),
                    begin
                        Base32edBytes = base32_clockwork:encode(RandomBytes),
                        case base32_clockwork:decode(Base32edBytes) of
                            {ok, RandomBytes} ->
                                true;
                            {ok, _} ->
                                false;
                            {error, _Reason} ->
                                false
                        end
                    end)).


proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).
