-module(base32_utils).

-export([rev_bits_list_to_binary/1, bits_list_size/1]).


rev_bits_list_to_binary(List) ->
    rev_bits_list_to_binary0(List, <<>>).


rev_bits_list_to_binary0([], Accu) ->
    Accu;
rev_bits_list_to_binary0([Bits | Next], Accu) ->
    rev_bits_list_to_binary0(Next, <<Bits/bitstring, Accu/bitstring>>).


bits_list_size(List) ->
    bits_list_size0(List, 0).


bits_list_size0([], Count) ->
    Count;
bits_list_size0([Bits | Next], Count) ->
    bits_list_size0(Next, bit_size(Bits) + Count).
