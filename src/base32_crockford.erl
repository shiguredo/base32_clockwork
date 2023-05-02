-module(base32_crockford).

-export([encode/1, encode_check/1, decode/1, decode_check/1]).

-import(base32_utils, [rev_bits_list_to_binary/1]).


data_to_integer(Data, Padding) ->
    data_to_integer0(Data, Padding, 0).


data_to_integer0(<<>>, _, N) ->
    N;
data_to_integer0(<<Bits:1>>, true, N) ->
    (N bsl 5) bor (Bits bsl 4);
data_to_integer0(<<Bits:2>>, true, N) ->
    (N bsl 5) bor (Bits bsl 3);
data_to_integer0(<<Bits:3>>, true, N) ->
    (N bsl 5) bor (Bits bsl 2);
data_to_integer0(<<Bits:4>>, true, N) ->
    (N bsl 5) bor (Bits bsl 1);
data_to_integer0(<<Bits:1>>, false, N) ->
    (N bsl 1) bor Bits;
data_to_integer0(<<Bits:2>>, false, N) ->
    (N bsl 2) bor Bits;
data_to_integer0(<<Bits:3>>, false, N) ->
    (N bsl 3) bor Bits;
data_to_integer0(<<Bits:4>>, false, N) ->
    (N bsl 4) bor Bits;
data_to_integer0(<<Bits:5, Next/bitstring>>, Padding, N) ->
    data_to_integer0(Next, Padding, (N bsl 5) bor Bits).


-spec encode(binary() | integer()) -> binary().
encode(Value) ->
    Encoded = encode0(Value, []),
    list_to_binary(Encoded).


-spec encode_check(binary()) -> binary().
encode_check(Data) when is_binary(Data) ->
    Check = check_symbol(data_to_integer(Data, false)),
    Encoded = encode0(Data, [Check]),
    list_to_binary(Encoded);
encode_check(Value) ->
    Check = check_symbol(Value),
    Encoded = encode0(Value, [Check]),
    list_to_binary(Encoded).


encode0(Data, Accu) when is_binary(Data) ->
    Size = size(Data) * 8,
    BaseCount = Size div 5,
    Count = case Size rem 5 of
                0 -> BaseCount;
                _ -> BaseCount + 1
            end,
    encode1(data_to_integer(Data, true), Count, Accu);
encode0(Value, Accu) ->
    BaseCount = Value div 32,
    Count = case Value rem 32 of
                0 -> BaseCount;
                _ -> BaseCount + 1
            end,
    encode1(Value, Count, Accu).


encode1(_, 0, Accu) ->
    Accu;
encode1(Value, Count, Accu) ->
    Next = Value div 32,
    SymVal = Value rem 32,
    encode1(Next, Count - 1, [symbol(SymVal) | Accu]).


check_symbol(Value) ->
    symbol(Value rem 37).


symbol(0) -> $0;
symbol(1) -> $1;
symbol(2) -> $2;
symbol(3) -> $3;
symbol(4) -> $4;
symbol(5) -> $5;
symbol(6) -> $6;
symbol(7) -> $7;
symbol(8) -> $8;
symbol(9) -> $9;
symbol(10) -> $A;
symbol(11) -> $B;
symbol(12) -> $C;
symbol(13) -> $D;
symbol(14) -> $E;
symbol(15) -> $F;
symbol(16) -> $G;
symbol(17) -> $H;
symbol(18) -> $J;
symbol(19) -> $K;
symbol(20) -> $M;
symbol(21) -> $N;
symbol(22) -> $P;
symbol(23) -> $Q;
symbol(24) -> $R;
symbol(25) -> $S;
symbol(26) -> $T;
symbol(27) -> $V;
symbol(28) -> $W;
symbol(29) -> $X;
symbol(30) -> $Y;
symbol(31) -> $Z;
symbol(32) -> $*;
symbol(33) -> $~;
symbol(34) -> $$;
symbol(35) -> $=;
symbol(36) -> $U.


-spec decode(binary()) -> binary().
decode(Data) when is_binary(Data) ->
    decode0(Data, []).


-spec decode_check(binary()) -> {ok, binary()} | {error, atom()}.
decode_check(Data) ->
    Size = (size(Data) - 1) * 8,
    <<Data0:Size/bitstring, Expectedcheck:8>> = Data,
    Decoded = decode(Data0),
    Check = check_symbol(data_to_integer(Decoded, false)),
    case Check =:= Expectedcheck of
        true -> {ok, Decoded};
        false -> {error, invalid}
    end.


decode0(<<>>, Accu) ->
    Decoded0 = rev_bits_list_to_binary(Accu),
    DecodedSize = bit_size(Decoded0),
    case DecodedSize rem 8 of
        0 ->
            Decoded0;
        PaddingSize ->
            DataSize = DecodedSize - PaddingSize,
            <<Decoded1:DataSize/bitstring, 0:PaddingSize>> = Decoded0,
            Decoded1
    end;
decode0(<<"0", Next/bitstring>>, Accu) ->
    decode0(Next, [<<0:5>> | Accu]);
decode0(<<"O", Next/bitstring>>, Accu) ->
    decode0(Next, [<<0:5>> | Accu]);
decode0(<<"o", Next/bitstring>>, Accu) ->
    decode0(Next, [<<0:5>> | Accu]);
decode0(<<"1", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>> | Accu]);
decode0(<<"I", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>> | Accu]);
decode0(<<"i", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>> | Accu]);
decode0(<<"L", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>> | Accu]);
decode0(<<"l", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>> | Accu]);
decode0(<<"2", Next/bitstring>>, Accu) ->
    decode0(Next, [<<2:5>> | Accu]);
decode0(<<"3", Next/bitstring>>, Accu) ->
    decode0(Next, [<<3:5>> | Accu]);
decode0(<<"4", Next/bitstring>>, Accu) ->
    decode0(Next, [<<4:5>> | Accu]);
decode0(<<"5", Next/bitstring>>, Accu) ->
    decode0(Next, [<<5:5>> | Accu]);
decode0(<<"6", Next/bitstring>>, Accu) ->
    decode0(Next, [<<6:5>> | Accu]);
decode0(<<"7", Next/bitstring>>, Accu) ->
    decode0(Next, [<<7:5>> | Accu]);
decode0(<<"8", Next/bitstring>>, Accu) ->
    decode0(Next, [<<8:5>> | Accu]);
decode0(<<"9", Next/bitstring>>, Accu) ->
    decode0(Next, [<<9:5>> | Accu]);
decode0(<<"A", Next/bitstring>>, Accu) ->
    decode0(Next, [<<10:5>> | Accu]);
decode0(<<"a", Next/bitstring>>, Accu) ->
    decode0(Next, [<<10:5>> | Accu]);
decode0(<<"B", Next/bitstring>>, Accu) ->
    decode0(Next, [<<11:5>> | Accu]);
decode0(<<"b", Next/bitstring>>, Accu) ->
    decode0(Next, [<<11:5>> | Accu]);
decode0(<<"C", Next/bitstring>>, Accu) ->
    decode0(Next, [<<12:5>> | Accu]);
decode0(<<"c", Next/bitstring>>, Accu) ->
    decode0(Next, [<<12:5>> | Accu]);
decode0(<<"D", Next/bitstring>>, Accu) ->
    decode0(Next, [<<13:5>> | Accu]);
decode0(<<"d", Next/bitstring>>, Accu) ->
    decode0(Next, [<<13:5>> | Accu]);
decode0(<<"E", Next/bitstring>>, Accu) ->
    decode0(Next, [<<14:5>> | Accu]);
decode0(<<"e", Next/bitstring>>, Accu) ->
    decode0(Next, [<<14:5>> | Accu]);
decode0(<<"F", Next/bitstring>>, Accu) ->
    decode0(Next, [<<15:5>> | Accu]);
decode0(<<"f", Next/bitstring>>, Accu) ->
    decode0(Next, [<<15:5>> | Accu]);
decode0(<<"G", Next/bitstring>>, Accu) ->
    decode0(Next, [<<16:5>> | Accu]);
decode0(<<"g", Next/bitstring>>, Accu) ->
    decode0(Next, [<<16:5>> | Accu]);
decode0(<<"H", Next/bitstring>>, Accu) ->
    decode0(Next, [<<17:5>> | Accu]);
decode0(<<"h", Next/bitstring>>, Accu) ->
    decode0(Next, [<<17:5>> | Accu]);
decode0(<<"J", Next/bitstring>>, Accu) ->
    decode0(Next, [<<18:5>> | Accu]);
decode0(<<"j", Next/bitstring>>, Accu) ->
    decode0(Next, [<<18:5>> | Accu]);
decode0(<<"K", Next/bitstring>>, Accu) ->
    decode0(Next, [<<19:5>> | Accu]);
decode0(<<"k", Next/bitstring>>, Accu) ->
    decode0(Next, [<<19:5>> | Accu]);
decode0(<<"M", Next/bitstring>>, Accu) ->
    decode0(Next, [<<20:5>> | Accu]);
decode0(<<"m", Next/bitstring>>, Accu) ->
    decode0(Next, [<<20:5>> | Accu]);
decode0(<<"N", Next/bitstring>>, Accu) ->
    decode0(Next, [<<21:5>> | Accu]);
decode0(<<"n", Next/bitstring>>, Accu) ->
    decode0(Next, [<<21:5>> | Accu]);
decode0(<<"P", Next/bitstring>>, Accu) ->
    decode0(Next, [<<22:5>> | Accu]);
decode0(<<"p", Next/bitstring>>, Accu) ->
    decode0(Next, [<<22:5>> | Accu]);
decode0(<<"Q", Next/bitstring>>, Accu) ->
    decode0(Next, [<<23:5>> | Accu]);
decode0(<<"q", Next/bitstring>>, Accu) ->
    decode0(Next, [<<23:5>> | Accu]);
decode0(<<"R", Next/bitstring>>, Accu) ->
    decode0(Next, [<<24:5>> | Accu]);
decode0(<<"r", Next/bitstring>>, Accu) ->
    decode0(Next, [<<24:5>> | Accu]);
decode0(<<"S", Next/bitstring>>, Accu) ->
    decode0(Next, [<<25:5>> | Accu]);
decode0(<<"s", Next/bitstring>>, Accu) ->
    decode0(Next, [<<25:5>> | Accu]);
decode0(<<"T", Next/bitstring>>, Accu) ->
    decode0(Next, [<<26:5>> | Accu]);
decode0(<<"t", Next/bitstring>>, Accu) ->
    decode0(Next, [<<26:5>> | Accu]);
decode0(<<"V", Next/bitstring>>, Accu) ->
    decode0(Next, [<<27:5>> | Accu]);
decode0(<<"v", Next/bitstring>>, Accu) ->
    decode0(Next, [<<27:5>> | Accu]);
decode0(<<"W", Next/bitstring>>, Accu) ->
    decode0(Next, [<<28:5>> | Accu]);
decode0(<<"w", Next/bitstring>>, Accu) ->
    decode0(Next, [<<28:5>> | Accu]);
decode0(<<"X", Next/bitstring>>, Accu) ->
    decode0(Next, [<<29:5>> | Accu]);
decode0(<<"x", Next/bitstring>>, Accu) ->
    decode0(Next, [<<29:5>> | Accu]);
decode0(<<"Y", Next/bitstring>>, Accu) ->
    decode0(Next, [<<30:5>> | Accu]);
decode0(<<"y", Next/bitstring>>, Accu) ->
    decode0(Next, [<<30:5>> | Accu]);
decode0(<<"Z", Next/bitstring>>, Accu) ->
    decode0(Next, [<<31:5>> | Accu]);
decode0(<<"z", Next/bitstring>>, Accu) ->
    decode0(Next, [<<31:5>> | Accu]);
decode0(<<"-", Next/bitstring>>, Accu) ->
    decode0(Next, Accu).
