-module(base32_utils).

-export([rev_bits_list_to_binary/1, bits_list_size/1]).


%% 入力の要素は連結する前に逆順に並べ替えられるため、返値はバイト境界に
%% 整っていない bitstring() になり得る
-spec rev_bits_list_to_binary([bitstring()]) -> bitstring().
rev_bits_list_to_binary(List) ->
    rev_bits_list_to_binary0(List, <<>>).


rev_bits_list_to_binary0([], Accu) ->
    Accu;
rev_bits_list_to_binary0([Bits | Next], Accu) ->
    rev_bits_list_to_binary0(Next, <<Bits/bitstring, Accu/bitstring>>).


%% リスト内の全要素のビット数の合計を返す
-spec bits_list_size([bitstring()]) -> non_neg_integer().
bits_list_size(List) ->
    bits_list_size0(List, 0).


bits_list_size0([], Count) ->
    Count;
bits_list_size0([Bits | Next], Count) ->
    bits_list_size0(Next, bit_size(Bits) + Count).
