-module(base32_clockwork).

-export([encode/1, decode/1]).

-import(base32_utils, [rev_bits_list_to_binary/1, bits_list_size/1]).

-spec encode(binary()) -> binary().
encode(Data) ->
    encode0(Data, []).

encode0(<<>>, Accu) ->
    list_to_binary(lists:reverse(Accu));
encode0(<<Bits:5, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits)|Accu]);
encode0(<<Bits:4, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 1)|Accu]);
encode0(<<Bits:3, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 2)|Accu]);
encode0(<<Bits:2, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 3)|Accu]);
encode0(<<Bits:1, Next/bitstring>>, Accu) ->
    encode0(Next, [symbol(Bits bsl 4)|Accu]).

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

-spec decode(binary()) -> {ok, binary()} | {error, atom()}.
decode(Data) ->
    decode0(Data, []).

decode0(<<>>, []) ->
    {ok, <<>>};
decode0(<<>>, Accu=[Last|Prev]) ->
    Size = bits_list_size(Accu),
    Last1 = case Size rem 8 of
                0 ->
                    Last;
                PaddingSize ->
                    BodySize = 5 - PaddingSize,
                    <<Last2:BodySize/bitstring, _/bitstring>> = Last,
                    Last2
            end,
    {ok, rev_bits_list_to_binary([Last1|Prev])};
decode0(<<_:8>>, []) ->
    {error, invalid_size};
decode0(<<"0", Next/binary>>, Accu) ->
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"O", Next/binary>>, Accu) ->
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"o", Next/binary>>, Accu) ->
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"1", Next/binary>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"I", Next/binary>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"i", Next/binary>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"L", Next/binary>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"l", Next/binary>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"2", Next/binary>>, Accu) ->
    decode0(Next, [<<2:5>>|Accu]);
decode0(<<"3", Next/binary>>, Accu) ->
    decode0(Next, [<<3:5>>|Accu]);
decode0(<<"4", Next/binary>>, Accu) ->
    decode0(Next, [<<4:5>>|Accu]);
decode0(<<"5", Next/binary>>, Accu) ->
    decode0(Next, [<<5:5>>|Accu]);
decode0(<<"6", Next/binary>>, Accu) ->
    decode0(Next, [<<6:5>>|Accu]);
decode0(<<"7", Next/binary>>, Accu) ->
    decode0(Next, [<<7:5>>|Accu]);
decode0(<<"8", Next/binary>>, Accu) ->
    decode0(Next, [<<8:5>>|Accu]);
decode0(<<"9", Next/binary>>, Accu) ->
    decode0(Next, [<<9:5>>|Accu]);
decode0(<<"A", Next/binary>>, Accu) ->
    decode0(Next, [<<10:5>>|Accu]);
decode0(<<"a", Next/binary>>, Accu) ->
    decode0(Next, [<<10:5>>|Accu]);
decode0(<<"B", Next/binary>>, Accu) ->
    decode0(Next, [<<11:5>>|Accu]);
decode0(<<"b", Next/binary>>, Accu) ->
    decode0(Next, [<<11:5>>|Accu]);
decode0(<<"C", Next/binary>>, Accu) ->
    decode0(Next, [<<12:5>>|Accu]);
decode0(<<"c", Next/binary>>, Accu) ->
    decode0(Next, [<<12:5>>|Accu]);
decode0(<<"D", Next/binary>>, Accu) ->
    decode0(Next, [<<13:5>>|Accu]);
decode0(<<"d", Next/binary>>, Accu) ->
    decode0(Next, [<<13:5>>|Accu]);
decode0(<<"E", Next/binary>>, Accu) ->
    decode0(Next, [<<14:5>>|Accu]);
decode0(<<"e", Next/binary>>, Accu) ->
    decode0(Next, [<<14:5>>|Accu]);
decode0(<<"F", Next/binary>>, Accu) ->
    decode0(Next, [<<15:5>>|Accu]);
decode0(<<"f", Next/binary>>, Accu) ->
    decode0(Next, [<<15:5>>|Accu]);
decode0(<<"G", Next/binary>>, Accu) ->
    decode0(Next, [<<16:5>>|Accu]);
decode0(<<"g", Next/binary>>, Accu) ->
    decode0(Next, [<<16:5>>|Accu]);
decode0(<<"H", Next/binary>>, Accu) ->
    decode0(Next, [<<17:5>>|Accu]);
decode0(<<"h", Next/binary>>, Accu) ->
    decode0(Next, [<<17:5>>|Accu]);
decode0(<<"J", Next/binary>>, Accu) ->
    decode0(Next, [<<18:5>>|Accu]);
decode0(<<"j", Next/binary>>, Accu) ->
    decode0(Next, [<<18:5>>|Accu]);
decode0(<<"K", Next/binary>>, Accu) ->
    decode0(Next, [<<19:5>>|Accu]);
decode0(<<"k", Next/binary>>, Accu) ->
    decode0(Next, [<<19:5>>|Accu]);
decode0(<<"M", Next/binary>>, Accu) ->
    decode0(Next, [<<20:5>>|Accu]);
decode0(<<"m", Next/binary>>, Accu) ->
    decode0(Next, [<<20:5>>|Accu]);
decode0(<<"N", Next/binary>>, Accu) ->
    decode0(Next, [<<21:5>>|Accu]);
decode0(<<"n", Next/binary>>, Accu) ->
    decode0(Next, [<<21:5>>|Accu]);
decode0(<<"P", Next/binary>>, Accu) ->
    decode0(Next, [<<22:5>>|Accu]);
decode0(<<"p", Next/binary>>, Accu) ->
    decode0(Next, [<<22:5>>|Accu]);
decode0(<<"Q", Next/binary>>, Accu) ->
    decode0(Next, [<<23:5>>|Accu]);
decode0(<<"q", Next/binary>>, Accu) ->
    decode0(Next, [<<23:5>>|Accu]);
decode0(<<"R", Next/binary>>, Accu) ->
    decode0(Next, [<<24:5>>|Accu]);
decode0(<<"r", Next/binary>>, Accu) ->
    decode0(Next, [<<24:5>>|Accu]);
decode0(<<"S", Next/binary>>, Accu) ->
    decode0(Next, [<<25:5>>|Accu]);
decode0(<<"s", Next/binary>>, Accu) ->
    decode0(Next, [<<25:5>>|Accu]);
decode0(<<"T", Next/binary>>, Accu) ->
    decode0(Next, [<<26:5>>|Accu]);
decode0(<<"t", Next/binary>>, Accu) ->
    decode0(Next, [<<26:5>>|Accu]);
decode0(<<"V", Next/binary>>, Accu) ->
    decode0(Next, [<<27:5>>|Accu]);
decode0(<<"v", Next/binary>>, Accu) ->
    decode0(Next, [<<27:5>>|Accu]);
decode0(<<"W", Next/binary>>, Accu) ->
    decode0(Next, [<<28:5>>|Accu]);
decode0(<<"w", Next/binary>>, Accu) ->
    decode0(Next, [<<28:5>>|Accu]);
decode0(<<"X", Next/binary>>, Accu) ->
    decode0(Next, [<<29:5>>|Accu]);
decode0(<<"x", Next/binary>>, Accu) ->
    decode0(Next, [<<29:5>>|Accu]);
decode0(<<"Y", Next/binary>>, Accu) ->
    decode0(Next, [<<30:5>>|Accu]);
decode0(<<"y", Next/binary>>, Accu) ->
    decode0(Next, [<<30:5>>|Accu]);
decode0(<<"Z", Next/binary>>, Accu) ->
    decode0(Next, [<<31:5>>|Accu]);
decode0(<<"z", Next/binary>>, Accu) ->
    decode0(Next, [<<31:5>>|Accu]);
decode0(_, _) ->
    {error, invalid_format}.

