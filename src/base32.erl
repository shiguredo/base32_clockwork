-module(base32).

-export([encode/2, decode/2]).
-export_type([base32_format/0]).

-type base32_format() :: rfc4648 | crockford | crockford_check | clockwork.


%% crockford と crockford_check のみ non_neg_integer() を受け付ける。
%% rfc4648 と clockwork に non_neg_integer() を渡すと function_clause になる。
-spec encode(base32_format(), binary() | non_neg_integer()) -> binary().
encode(rfc4648, Data) ->
    base32_rfc4648:encode(Data);
encode(crockford, Data) ->
    base32_crockford:encode(Data);
encode(crockford_check, Data) ->
    base32_crockford:encode_check(Data);
encode(clockwork, Data) ->
    base32_clockwork:encode(Data).


-spec decode(base32_format(), binary()) ->
          {ok, binary()} | {error, invalid_format}.
decode(rfc4648, Data) ->
    base32_rfc4648:decode(Data);
decode(crockford, Data) ->
    base32_crockford:decode(Data);
decode(crockford_check, Data) ->
    base32_crockford:decode_check(Data);
decode(clockwork, Data) ->
    base32_clockwork:decode(Data).
