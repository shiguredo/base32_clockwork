-module(base32).

-export([encode/2, decode/2]).

-type base32_format() :: rfc4648 | crockford | crockford_check.

-spec encode(base32_format(), binary()) -> binary().
encode(rfc4648, Data) ->
    base32_rfc4648:encode(Data);
encode(crockford, Data) ->
    base32_crockford:encode(Data, false);
encode(crockford_check, Data) ->
    base32_crockford:encode(Data, true).

-spec decode(base32_format(), binary()) -> binary().
decode(rfc4648, Data) ->
    base32_rfc4648:decode(Data).
%% decode(crockford, Data) ->
%%     base32_crockford:decode(Data, false);
%% decode(crockford_check, Data) ->
%%     base32_crockford:decode(Data, true).

