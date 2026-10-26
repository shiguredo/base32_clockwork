-module(base32_clockwork).

-export([encode/1, decode/1]).


-spec encode(binary()) -> binary().
encode(Data) ->
    encode0(Data, []).


encode0(<<>>, Accu) ->
    list_to_binary(lists:reverse(Accu));
encode0(<<Bits:5, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits) | Accu]);
encode0(<<Bits:4, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 1) | Accu]);
encode0(<<Bits:3, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 2) | Accu]);
encode0(<<Bits:2, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 3) | Accu]);
encode0(<<Bits:1, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 4) | Accu]).


symbol(0) ->
    $0;
symbol(1) ->
    $1;
symbol(2) ->
    $2;
symbol(3) ->
    $3;
symbol(4) ->
    $4;
symbol(5) ->
    $5;
symbol(6) ->
    $6;
symbol(7) ->
    $7;
symbol(8) ->
    $8;
symbol(9) ->
    $9;
symbol(10) ->
    $A;
symbol(11) ->
    $B;
symbol(12) ->
    $C;
symbol(13) ->
    $D;
symbol(14) ->
    $E;
symbol(15) ->
    $F;
symbol(16) ->
    $G;
symbol(17) ->
    $H;
symbol(18) ->
    $J;
symbol(19) ->
    $K;
symbol(20) ->
    $M;
symbol(21) ->
    $N;
symbol(22) ->
    $P;
symbol(23) ->
    $Q;
symbol(24) ->
    $R;
symbol(25) ->
    $S;
symbol(26) ->
    $T;
symbol(27) ->
    $V;
symbol(28) ->
    $W;
symbol(29) ->
    $X;
symbol(30) ->
    $Y;
symbol(31) ->
    $Z.


-spec decode(binary()) -> {ok, binary()} | {error, invalid_format}.
decode(<<>>) ->
    {ok, <<>>};
%% シンボル 1 文字 (5 ビット) では 1 バイトに満たないため、形式として不完全な入力として拒否する
decode(<<_>>) ->
    {error, invalid_format};
decode(Data) ->
    decode0(Data, base32_utils:new_decode_acc()).


decode0(<<>>, Acc) ->
    base32_utils:finalize_decode(Acc);
decode0(<<"0", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(0, Acc));
decode0(<<"O", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(0, Acc));
decode0(<<"o", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(0, Acc));
decode0(<<"1", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(1, Acc));
decode0(<<"I", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(1, Acc));
decode0(<<"i", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(1, Acc));
decode0(<<"L", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(1, Acc));
decode0(<<"l", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(1, Acc));
decode0(<<"2", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(2, Acc));
decode0(<<"3", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(3, Acc));
decode0(<<"4", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(4, Acc));
decode0(<<"5", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(5, Acc));
decode0(<<"6", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(6, Acc));
decode0(<<"7", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(7, Acc));
decode0(<<"8", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(8, Acc));
decode0(<<"9", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(9, Acc));
decode0(<<"A", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(10, Acc));
decode0(<<"a", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(10, Acc));
decode0(<<"B", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(11, Acc));
decode0(<<"b", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(11, Acc));
decode0(<<"C", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(12, Acc));
decode0(<<"c", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(12, Acc));
decode0(<<"D", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(13, Acc));
decode0(<<"d", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(13, Acc));
decode0(<<"E", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(14, Acc));
decode0(<<"e", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(14, Acc));
decode0(<<"F", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(15, Acc));
decode0(<<"f", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(15, Acc));
decode0(<<"G", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(16, Acc));
decode0(<<"g", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(16, Acc));
decode0(<<"H", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(17, Acc));
decode0(<<"h", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(17, Acc));
decode0(<<"J", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(18, Acc));
decode0(<<"j", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(18, Acc));
decode0(<<"K", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(19, Acc));
decode0(<<"k", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(19, Acc));
decode0(<<"M", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(20, Acc));
decode0(<<"m", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(20, Acc));
decode0(<<"N", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(21, Acc));
decode0(<<"n", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(21, Acc));
decode0(<<"P", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(22, Acc));
decode0(<<"p", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(22, Acc));
decode0(<<"Q", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(23, Acc));
decode0(<<"q", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(23, Acc));
decode0(<<"R", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(24, Acc));
decode0(<<"r", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(24, Acc));
decode0(<<"S", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(25, Acc));
decode0(<<"s", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(25, Acc));
decode0(<<"T", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(26, Acc));
decode0(<<"t", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(26, Acc));
decode0(<<"V", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(27, Acc));
decode0(<<"v", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(27, Acc));
decode0(<<"W", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(28, Acc));
decode0(<<"w", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(28, Acc));
decode0(<<"X", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(29, Acc));
decode0(<<"x", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(29, Acc));
decode0(<<"Y", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(30, Acc));
decode0(<<"y", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(30, Acc));
decode0(<<"Z", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(31, Acc));
decode0(<<"z", Next/binary>>, Acc) ->
    decode0(Next, base32_utils:push_symbol(31, Acc));
decode0(_, _) ->
    {error, invalid_format}.
