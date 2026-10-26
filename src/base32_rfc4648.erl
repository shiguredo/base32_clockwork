-module(base32_rfc4648).

-export([encode/1, decode/1]).


-spec encode(binary()) -> binary().
encode(Data) ->
    list_to_binary(encode0(Data)).


encode0(Data) ->
    Size = bit_size(Data),
    RevEncoded = case 40 - (Size rem 40) of
                     40 ->
                         encode1(Data, []);
                     32 ->
                         [$=, $=, $=, $=, $=, $= | encode1(Data, [])];
                     24 ->
                         [$=, $=, $=, $= | encode1(Data, [])];
                     16 ->
                         [$=, $=, $= | encode1(Data, [])];
                     8 ->
                         [$= | encode1(Data, [])]
                 end,
    lists:reverse(RevEncoded).


encode1(<<>>, Accu) ->
    Accu;

encode1(<<0:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$A | Accu]);
encode1(<<1:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$B | Accu]);
encode1(<<2:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$C | Accu]);
encode1(<<3:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$D | Accu]);
encode1(<<4:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$E | Accu]);
encode1(<<5:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$F | Accu]);
encode1(<<6:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$G | Accu]);
encode1(<<7:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$H | Accu]);
encode1(<<8:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$I | Accu]);
encode1(<<9:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$J | Accu]);
encode1(<<10:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$K | Accu]);
encode1(<<11:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$L | Accu]);
encode1(<<12:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$M | Accu]);
encode1(<<13:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$N | Accu]);
encode1(<<14:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$O | Accu]);
encode1(<<15:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$P | Accu]);
encode1(<<16:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$Q | Accu]);
encode1(<<17:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$R | Accu]);
encode1(<<18:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$S | Accu]);
encode1(<<19:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$T | Accu]);
encode1(<<20:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$U | Accu]);
encode1(<<21:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$V | Accu]);
encode1(<<22:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$W | Accu]);
encode1(<<23:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$X | Accu]);
encode1(<<24:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$Y | Accu]);
encode1(<<25:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$Z | Accu]);
encode1(<<26:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$2 | Accu]);
encode1(<<27:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$3 | Accu]);
encode1(<<28:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$4 | Accu]);
encode1(<<29:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$5 | Accu]);
encode1(<<30:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$6 | Accu]);
encode1(<<31:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$7 | Accu]);

encode1(<<0:4>>, Accu) ->
    [$A | Accu];
encode1(<<1:4>>, Accu) ->
    [$C | Accu];
encode1(<<2:4>>, Accu) ->
    [$E | Accu];
encode1(<<3:4>>, Accu) ->
    [$G | Accu];
encode1(<<4:4>>, Accu) ->
    [$I | Accu];
encode1(<<5:4>>, Accu) ->
    [$K | Accu];
encode1(<<6:4>>, Accu) ->
    [$M | Accu];
encode1(<<7:4>>, Accu) ->
    [$O | Accu];
encode1(<<8:4>>, Accu) ->
    [$Q | Accu];
encode1(<<9:4>>, Accu) ->
    [$S | Accu];
encode1(<<10:4>>, Accu) ->
    [$U | Accu];
encode1(<<11:4>>, Accu) ->
    [$W | Accu];
encode1(<<12:4>>, Accu) ->
    [$Y | Accu];
encode1(<<13:4>>, Accu) ->
    [$2 | Accu];
encode1(<<14:4>>, Accu) ->
    [$4 | Accu];
encode1(<<15:4>>, Accu) ->
    [$6 | Accu];

encode1(<<0:3>>, Accu) ->
    [$A | Accu];
encode1(<<1:3>>, Accu) ->
    [$E | Accu];
encode1(<<2:3>>, Accu) ->
    [$I | Accu];
encode1(<<3:3>>, Accu) ->
    [$M | Accu];
encode1(<<4:3>>, Accu) ->
    [$Q | Accu];
encode1(<<5:3>>, Accu) ->
    [$U | Accu];
encode1(<<6:3>>, Accu) ->
    [$Y | Accu];
encode1(<<7:3>>, Accu) ->
    [$4 | Accu];

encode1(<<0:2>>, Accu) ->
    [$A | Accu];
encode1(<<1:2>>, Accu) ->
    [$I | Accu];
encode1(<<2:2>>, Accu) ->
    [$Q | Accu];
encode1(<<3:2>>, Accu) ->
    [$Y | Accu];

encode1(<<0:1>>, Accu) ->
    [$A | Accu];
encode1(<<1:1>>, Accu) ->
    [$Q | Accu].


-spec decode(binary()) -> {ok, binary()} | {error, invalid_format}.
decode(Data) ->
    %% パディングが最後のシンボルを書き換えるため、確定は次シンボル到着まで遅らせる
    decode0(Data, {0, 0, []}).


decode0(<<>>, Acc) ->
    finalize_bits(Acc);
decode0(<<"A", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(0, Acc));
decode0(<<"B", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(1, Acc));
decode0(<<"C", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(2, Acc));
decode0(<<"D", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(3, Acc));
decode0(<<"E", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(4, Acc));
decode0(<<"F", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(5, Acc));
decode0(<<"G", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(6, Acc));
decode0(<<"H", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(7, Acc));
decode0(<<"I", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(8, Acc));
decode0(<<"J", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(9, Acc));
decode0(<<"K", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(10, Acc));
decode0(<<"L", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(11, Acc));
decode0(<<"M", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(12, Acc));
decode0(<<"N", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(13, Acc));
decode0(<<"O", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(14, Acc));
decode0(<<"P", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(15, Acc));
decode0(<<"Q", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(16, Acc));
decode0(<<"R", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(17, Acc));
decode0(<<"S", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(18, Acc));
decode0(<<"T", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(19, Acc));
decode0(<<"U", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(20, Acc));
decode0(<<"V", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(21, Acc));
decode0(<<"W", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(22, Acc));
decode0(<<"X", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(23, Acc));
decode0(<<"Y", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(24, Acc));
decode0(<<"Z", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(25, Acc));
decode0(<<"2", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(26, Acc));
decode0(<<"3", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(27, Acc));
decode0(<<"4", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(28, Acc));
decode0(<<"5", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(29, Acc));
decode0(<<"6", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(30, Acc));
decode0(<<"7", Next/bitstring>>, Acc) ->
    decode0(Next, push_symbol(31, Acc));

decode0(<<"======">>, Acc) ->
    finalize_padding(Acc, 3);
decode0(<<"====">>, Acc) ->
    finalize_padding(Acc, 1);
decode0(<<"===">>, Acc) ->
    finalize_padding(Acc, 4);
decode0(<<"=">>, Acc) ->
    finalize_padding(Acc, 2);

decode0(_, _) ->
    {error, invalid_format}.


%% 5 ビットシンボルを追加する。
%% パディングは最後のシンボルを書き換えるため、8 シンボルちょうどでは確定しない。
%% 次のシンボルが来た時点で、先行する 40 ビットを 5 byte として確定する。
push_symbol(Value, {Acc, 40, Out}) ->
    {Value, 5, [<<Acc:40>> | Out]};
push_symbol(Value, {Acc, BitCount, Out}) ->
    {(Acc bsl 5) bor Value, BitCount + 5, Out}.


%% 最後の 5 ビットシンボルの下位ビットが 0 なら、上位 KeepBits ビットだけ残して終える。
%% 下位ビットが 0 でない入力は invalid_format を返す。
finalize_padding(Acc, KeepBits) ->
    Drop = 5 - KeepBits,
    case take_last_symbol(Acc) of
        {Last, Rest} when (Last band ((1 bsl Drop) - 1)) =:= 0 ->
            finalize_bits(push_bits(Last bsr Drop, KeepBits, Rest));
        _ ->
            {error, invalid_format}
    end.


take_last_symbol({Acc, BitCount, Out}) when BitCount >= 5 ->
    {Acc band 31, {Acc bsr 5, BitCount - 5, Out}};
take_last_symbol(_) ->
    error.


push_bits(Value, Len, {Acc, BitCount, Out}) ->
    {(Acc bsl Len) bor Value, BitCount + Len, Out}.


%% パディングなしの端数はバイト境界へ切り詰めず、ビット列のまま返す。
finalize_bits({_Acc, 0, Out}) ->
    {ok, iolist_to_binary(lists:reverse(Out))};
finalize_bits({Acc, BitCount, Out}) ->
    Prefix = iolist_to_binary(lists:reverse(Out)),
    {ok, <<Prefix/binary, Acc:BitCount>>}.
