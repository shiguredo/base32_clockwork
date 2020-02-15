-module(base32_utils).

-export([rev_bits_list_to_binary/1]).

rev_bits_list_to_binary(List) ->
    rev_bits_list_to_binary0(List, <<>>).

rev_bits_list_to_binary0([], Accu) ->
    Accu;
rev_bits_list_to_binary0([Bits|Next], Accu) ->
    rev_bits_list_to_binary0(Next, <<Bits/bitstring, Accu/bitstring>>).

