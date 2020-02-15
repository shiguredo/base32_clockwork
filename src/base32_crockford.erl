-module(base32_crockford).

-export([encode/2]).

-spec encode(binary(), boolean()) -> binary().
encode(Data, Checks) ->
    Encoded0 = encode0(Data, []),
    Encoded1 = case Checks of
                   false ->
                       Encoded0;
                   true ->
                       [checksum(Data)|Encoded0]
               end,
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
    case to_number(Data, 0) rem 37 of
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

to_number(<<>>, N) ->
    N;
to_number(<<Byte:8, Next/bitstring>>, N) ->
    to_number(Next, (N bsl 8) bor Byte).
