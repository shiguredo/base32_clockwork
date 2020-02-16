-module(base32_crockford).

-export([encode/1, encode_check/1, decode/1, decode_check/1]).

-import(base32_utils, [rev_bits_list_to_binary/1]).

-spec encode(binary()) -> binary().
encode(Data) ->
    Encoded = encode0(Data, []),
    list_to_binary(lists:reverse(Encoded)).

-spec encode_check(binary()) -> binary().
encode_check(Data) ->
    Encoded0 = encode0(Data, []),
    Encoded1 = [checksum(Data)|Encoded0],
    list_to_binary(lists:reverse(Encoded1)).

encode0(<<>>, Accu) ->
    Accu;
encode0(<<0:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$0|Accu]);
encode0(<<1:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$1|Accu]);
encode0(<<2:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$2|Accu]);
encode0(<<3:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$3|Accu]);
encode0(<<4:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$4|Accu]);
encode0(<<5:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$5|Accu]);
encode0(<<6:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$6|Accu]);
encode0(<<7:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$7|Accu]);
encode0(<<8:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$8|Accu]);
encode0(<<9:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$9|Accu]);
encode0(<<10:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$A|Accu]);
encode0(<<11:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$B|Accu]);
encode0(<<12:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$C|Accu]);
encode0(<<13:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$D|Accu]);
encode0(<<14:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$E|Accu]);
encode0(<<15:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$F|Accu]);
encode0(<<16:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$G|Accu]);
encode0(<<17:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$H|Accu]);
encode0(<<18:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$J|Accu]);
encode0(<<19:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$K|Accu]);
encode0(<<20:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$M|Accu]);
encode0(<<21:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$N|Accu]);
encode0(<<22:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$P|Accu]);
encode0(<<23:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$Q|Accu]);
encode0(<<24:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$R|Accu]);
encode0(<<25:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$S|Accu]);
encode0(<<26:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$T|Accu]);
encode0(<<27:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$V|Accu]);
encode0(<<28:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$W|Accu]);
encode0(<<29:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$X|Accu]);
encode0(<<30:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$Y|Accu]);
encode0(<<31:5, Next/bitstring>>, Accu) ->
    encode0(Next, [$Z|Accu]);
encode0(<<0:4>>, Accu) ->
    [$0|Accu];
encode0(<<1:4>>, Accu) ->
    [$2|Accu];
encode0(<<2:4>>, Accu) ->
    [$4|Accu];
encode0(<<3:4>>, Accu) ->
    [$6|Accu];
encode0(<<4:4>>, Accu) ->
    [$8|Accu];
encode0(<<5:4>>, Accu) ->
    [$A|Accu];
encode0(<<6:4>>, Accu) ->
    [$C|Accu];
encode0(<<7:4>>, Accu) ->
    [$E|Accu];
encode0(<<8:4>>, Accu) ->
    [$G|Accu];
encode0(<<9:4>>, Accu) ->
    [$J|Accu];
encode0(<<10:4>>, Accu) ->
    [$M|Accu];
encode0(<<11:4>>, Accu) ->
    [$P|Accu];
encode0(<<12:4>>, Accu) ->
    [$R|Accu];
encode0(<<13:4>>, Accu) ->
    [$T|Accu];
encode0(<<14:4>>, Accu) ->
    [$W|Accu];
encode0(<<15:4>>, Accu) ->
    [$Y|Accu];
encode0(<<0:3>>, Accu) ->
    [$0|Accu];
encode0(<<1:3>>, Accu) ->
    [$4|Accu];
encode0(<<2:3>>, Accu) ->
    [$8|Accu];
encode0(<<3:3>>, Accu) ->
    [$C|Accu];
encode0(<<4:3>>, Accu) ->
    [$G|Accu];
encode0(<<5:3>>, Accu) ->
    [$M|Accu];
encode0(<<6:3>>, Accu) ->
    [$R|Accu];
encode0(<<7:3>>, Accu) ->
    [$W|Accu];
encode0(<<0:2>>, Accu) ->
    [$0|Accu];
encode0(<<1:2>>, Accu) ->
    [$8|Accu];
encode0(<<2:2>>, Accu) ->
    [$G|Accu];
encode0(<<3:2>>, Accu) ->
    [$R|Accu];
encode0(<<0:1>>, Accu) ->
    [$0|Accu];
encode0(<<1:1>>, Accu) ->
    [$G|Accu].

-spec checksum(binary()) -> char().
checksum(Data) ->
    number_to_checksum(binary_to_number(Data, 0)).

binary_to_number(<<>>, N) ->
    N;
binary_to_number(<<Byte:8, Next/bitstring>>, N) ->
    binary_to_number(Next, (N bsl 8) bor Byte).

number_to_checksum(N) ->
    case N rem 37 of
        0 -> $0;
        1 -> $1;
        2 -> $2;
        3 -> $3;
        4 -> $4;
        5 -> $5;
        6 -> $6;
        7 -> $7;
        8 -> $8;
        9 -> $9;
        10 -> $A;
        11 -> $B;
        12 -> $C;
        13 -> $D;
        14 -> $E;
        15 -> $F;
        16 -> $G;
        17 -> $H;
        18 -> $J;
        19 -> $K;
        20 -> $M;
        21 -> $N;
        22 -> $P;
        23 -> $Q;
        24 -> $R;
        25 -> $S;
        26 -> $T;
        27 -> $V;
        28 -> $W;
        29 -> $X;
        30 -> $Y;
        31 -> $Z;
        32 -> $*;
        33 -> $~;
        34 -> $$;
        35 -> $=;
        36 -> $U
    end.

-spec decode(binary()) -> binary().
decode(Data) ->
    decode0(Data, []).

-spec decode_check(binary()) -> {ok, binary()} | {error, atom()}.
decode_check(Data) ->
    Size = (size(Data) - 1) * 8,
    <<Data0:Size/bitstring, ExpectedCheckSum:8>> = Data,
    Decoded = decode(Data0),
    CheckSum = checksum(Decoded),
    case CheckSum =:= ExpectedCheckSum of
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
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"O", Next/bitstring>>, Accu) ->
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"o", Next/bitstring>>, Accu) ->
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"1", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"I", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"i", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"L", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"l", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"2", Next/bitstring>>, Accu) ->
    decode0(Next, [<<2:5>>|Accu]);
decode0(<<"3", Next/bitstring>>, Accu) ->
    decode0(Next, [<<3:5>>|Accu]);
decode0(<<"4", Next/bitstring>>, Accu) ->
    decode0(Next, [<<4:5>>|Accu]);
decode0(<<"5", Next/bitstring>>, Accu) ->
    decode0(Next, [<<5:5>>|Accu]);
decode0(<<"6", Next/bitstring>>, Accu) ->
    decode0(Next, [<<6:5>>|Accu]);
decode0(<<"7", Next/bitstring>>, Accu) ->
    decode0(Next, [<<7:5>>|Accu]);
decode0(<<"8", Next/bitstring>>, Accu) ->
    decode0(Next, [<<8:5>>|Accu]);
decode0(<<"9", Next/bitstring>>, Accu) ->
    decode0(Next, [<<9:5>>|Accu]);
decode0(<<"A", Next/bitstring>>, Accu) ->
    decode0(Next, [<<10:5>>|Accu]);
decode0(<<"a", Next/bitstring>>, Accu) ->
    decode0(Next, [<<10:5>>|Accu]);
decode0(<<"B", Next/bitstring>>, Accu) ->
    decode0(Next, [<<11:5>>|Accu]);
decode0(<<"b", Next/bitstring>>, Accu) ->
    decode0(Next, [<<11:5>>|Accu]);
decode0(<<"C", Next/bitstring>>, Accu) ->
    decode0(Next, [<<12:5>>|Accu]);
decode0(<<"c", Next/bitstring>>, Accu) ->
    decode0(Next, [<<12:5>>|Accu]);
decode0(<<"D", Next/bitstring>>, Accu) ->
    decode0(Next, [<<13:5>>|Accu]);
decode0(<<"d", Next/bitstring>>, Accu) ->
    decode0(Next, [<<13:5>>|Accu]);
decode0(<<"E", Next/bitstring>>, Accu) ->
    decode0(Next, [<<14:5>>|Accu]);
decode0(<<"e", Next/bitstring>>, Accu) ->
    decode0(Next, [<<14:5>>|Accu]);
decode0(<<"F", Next/bitstring>>, Accu) ->
    decode0(Next, [<<15:5>>|Accu]);
decode0(<<"f", Next/bitstring>>, Accu) ->
    decode0(Next, [<<15:5>>|Accu]);
decode0(<<"G", Next/bitstring>>, Accu) ->
    decode0(Next, [<<16:5>>|Accu]);
decode0(<<"g", Next/bitstring>>, Accu) ->
    decode0(Next, [<<16:5>>|Accu]);
decode0(<<"H", Next/bitstring>>, Accu) ->
    decode0(Next, [<<17:5>>|Accu]);
decode0(<<"h", Next/bitstring>>, Accu) ->
    decode0(Next, [<<17:5>>|Accu]);
decode0(<<"J", Next/bitstring>>, Accu) ->
    decode0(Next, [<<18:5>>|Accu]);
decode0(<<"j", Next/bitstring>>, Accu) ->
    decode0(Next, [<<18:5>>|Accu]);
decode0(<<"K", Next/bitstring>>, Accu) ->
    decode0(Next, [<<19:5>>|Accu]);
decode0(<<"k", Next/bitstring>>, Accu) ->
    decode0(Next, [<<19:5>>|Accu]);
decode0(<<"M", Next/bitstring>>, Accu) ->
    decode0(Next, [<<20:5>>|Accu]);
decode0(<<"m", Next/bitstring>>, Accu) ->
    decode0(Next, [<<20:5>>|Accu]);
decode0(<<"N", Next/bitstring>>, Accu) ->
    decode0(Next, [<<21:5>>|Accu]);
decode0(<<"n", Next/bitstring>>, Accu) ->
    decode0(Next, [<<21:5>>|Accu]);
decode0(<<"P", Next/bitstring>>, Accu) ->
    decode0(Next, [<<22:5>>|Accu]);
decode0(<<"p", Next/bitstring>>, Accu) ->
    decode0(Next, [<<22:5>>|Accu]);
decode0(<<"Q", Next/bitstring>>, Accu) ->
    decode0(Next, [<<23:5>>|Accu]);
decode0(<<"q", Next/bitstring>>, Accu) ->
    decode0(Next, [<<23:5>>|Accu]);
decode0(<<"R", Next/bitstring>>, Accu) ->
    decode0(Next, [<<24:5>>|Accu]);
decode0(<<"r", Next/bitstring>>, Accu) ->
    decode0(Next, [<<24:5>>|Accu]);
decode0(<<"S", Next/bitstring>>, Accu) ->
    decode0(Next, [<<25:5>>|Accu]);
decode0(<<"s", Next/bitstring>>, Accu) ->
    decode0(Next, [<<25:5>>|Accu]);
decode0(<<"T", Next/bitstring>>, Accu) ->
    decode0(Next, [<<26:5>>|Accu]);
decode0(<<"t", Next/bitstring>>, Accu) ->
    decode0(Next, [<<26:5>>|Accu]);
decode0(<<"V", Next/bitstring>>, Accu) ->
    decode0(Next, [<<27:5>>|Accu]);
decode0(<<"v", Next/bitstring>>, Accu) ->
    decode0(Next, [<<27:5>>|Accu]);
decode0(<<"W", Next/bitstring>>, Accu) ->
    decode0(Next, [<<28:5>>|Accu]);
decode0(<<"w", Next/bitstring>>, Accu) ->
    decode0(Next, [<<28:5>>|Accu]);
decode0(<<"X", Next/bitstring>>, Accu) ->
    decode0(Next, [<<29:5>>|Accu]);
decode0(<<"x", Next/bitstring>>, Accu) ->
    decode0(Next, [<<29:5>>|Accu]);
decode0(<<"Y", Next/bitstring>>, Accu) ->
    decode0(Next, [<<30:5>>|Accu]);
decode0(<<"y", Next/bitstring>>, Accu) ->
    decode0(Next, [<<30:5>>|Accu]);
decode0(<<"Z", Next/bitstring>>, Accu) ->
    decode0(Next, [<<31:5>>|Accu]);
decode0(<<"z", Next/bitstring>>, Accu) ->
    decode0(Next, [<<31:5>>|Accu]);
decode0(<<"-", Next/bitstring>>, Accu) ->
    decode0(Next, Accu).

