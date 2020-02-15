-module(base32_rfc4648).

-export([encode/1, decode/1]).

-import(base32_utils, [rev_bits_list_to_binary/1]).

-spec encode(binary()) -> binary().
encode(Data) ->
    list_to_binary(encode0(Data)).

encode0(Data) ->
    Size = bit_size(Data),
    RevEncoded = case 40 - (Size rem 40) of
        40 ->
            encode1(Data, []);
        32 ->
            [$=, $=, $=, $=, $=, $=|encode1(Data, [])];
        24 ->
            [$=, $=, $=, $=|encode1(Data, [])];
        16 ->
            [$=, $=, $=|encode1(Data, [])];
        8 ->
            [$=|encode1(Data, [])]
    end,
    lists:reverse(RevEncoded).

encode1(<<>>, Accu) ->
    Accu;

encode1(<<0:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$A|Accu]);
encode1(<<1:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$B|Accu]);
encode1(<<2:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$C|Accu]);
encode1(<<3:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$D|Accu]);
encode1(<<4:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$E|Accu]);
encode1(<<5:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$F|Accu]);
encode1(<<6:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$G|Accu]);
encode1(<<7:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$H|Accu]);
encode1(<<8:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$I|Accu]);
encode1(<<9:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$J|Accu]);
encode1(<<10:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$K|Accu]);
encode1(<<11:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$L|Accu]);
encode1(<<12:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$M|Accu]);
encode1(<<13:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$N|Accu]);
encode1(<<14:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$O|Accu]);
encode1(<<15:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$P|Accu]);
encode1(<<16:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$Q|Accu]);
encode1(<<17:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$R|Accu]);
encode1(<<18:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$S|Accu]);
encode1(<<19:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$T|Accu]);
encode1(<<20:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$U|Accu]);
encode1(<<21:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$V|Accu]);
encode1(<<22:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$W|Accu]);
encode1(<<23:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$X|Accu]);
encode1(<<24:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$Y|Accu]);
encode1(<<25:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$Z|Accu]);
encode1(<<26:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$2|Accu]);
encode1(<<27:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$3|Accu]);
encode1(<<28:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$4|Accu]);
encode1(<<29:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$5|Accu]);
encode1(<<30:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$6|Accu]);
encode1(<<31:5, Next/bitstring>>, Accu) ->
    encode1(Next, [$7|Accu]);

encode1(<<0:4>>, Accu) ->
    [$A|Accu];
encode1(<<1:4>>, Accu) ->
    [$C|Accu];
encode1(<<2:4>>, Accu) ->
    [$E|Accu];
encode1(<<3:4>>, Accu) ->
    [$G|Accu];
encode1(<<4:4>>, Accu) ->
    [$I|Accu];
encode1(<<5:4>>, Accu) ->
    [$K|Accu];
encode1(<<6:4>>, Accu) ->
    [$M|Accu];
encode1(<<7:4>>, Accu) ->
    [$O|Accu];
encode1(<<8:4>>, Accu) ->
    [$Q|Accu];
encode1(<<9:4>>, Accu) ->
    [$S|Accu];
encode1(<<10:4>>, Accu) ->
    [$U|Accu];
encode1(<<11:4>>, Accu) ->
    [$W|Accu];
encode1(<<12:4>>, Accu) ->
    [$Y|Accu];
encode1(<<13:4>>, Accu) ->
    [$2|Accu];
encode1(<<14:4>>, Accu) ->
    [$4|Accu];
encode1(<<15:4>>, Accu) ->
    [$6|Accu];

encode1(<<0:3>>, Accu) ->
    [$A|Accu];
encode1(<<1:3>>, Accu) ->
    [$E|Accu];
encode1(<<2:3>>, Accu) ->
    [$I|Accu];
encode1(<<3:3>>, Accu) ->
    [$M|Accu];
encode1(<<4:3>>, Accu) ->
    [$Q|Accu];
encode1(<<5:3>>, Accu) ->
    [$U|Accu];
encode1(<<6:3>>, Accu) ->
    [$Y|Accu];
encode1(<<7:3>>, Accu) ->
    [$4|Accu];

encode1(<<0:2>>, Accu) ->
    [$A|Accu];
encode1(<<1:2>>, Accu) ->
    [$I|Accu];
encode1(<<2:2>>, Accu) ->
    [$Q|Accu];
encode1(<<3:2>>, Accu) ->
    [$Y|Accu];

encode1(<<0:1>>, Accu) ->
    [$A|Accu];
encode1(<<1:1>>, Accu) ->
    [$Q|Accu].

-spec decode(binary()) -> binary().
decode(Data) ->
    decode0(Data, []).

decode0(<<>>, Accu) ->
    rev_bits_list_to_binary(Accu);
decode0(<<"A", Next/bitstring>>, Accu) ->
    decode0(Next, [<<0:5>>|Accu]);
decode0(<<"B", Next/bitstring>>, Accu) ->
    decode0(Next, [<<1:5>>|Accu]);
decode0(<<"C", Next/bitstring>>, Accu) ->
    decode0(Next, [<<2:5>>|Accu]);
decode0(<<"D", Next/bitstring>>, Accu) ->
    decode0(Next, [<<3:5>>|Accu]);
decode0(<<"E", Next/bitstring>>, Accu) ->
    decode0(Next, [<<4:5>>|Accu]);
decode0(<<"F", Next/bitstring>>, Accu) ->
    decode0(Next, [<<5:5>>|Accu]);
decode0(<<"G", Next/bitstring>>, Accu) ->
    decode0(Next, [<<6:5>>|Accu]);
decode0(<<"H", Next/bitstring>>, Accu) ->
    decode0(Next, [<<7:5>>|Accu]);
decode0(<<"I", Next/bitstring>>, Accu) ->
    decode0(Next, [<<8:5>>|Accu]);
decode0(<<"J", Next/bitstring>>, Accu) ->
    decode0(Next, [<<9:5>>|Accu]);
decode0(<<"K", Next/bitstring>>, Accu) ->
    decode0(Next, [<<10:5>>|Accu]);
decode0(<<"L", Next/bitstring>>, Accu) ->
    decode0(Next, [<<11:5>>|Accu]);
decode0(<<"M", Next/bitstring>>, Accu) ->
    decode0(Next, [<<12:5>>|Accu]);
decode0(<<"N", Next/bitstring>>, Accu) ->
    decode0(Next, [<<13:5>>|Accu]);
decode0(<<"O", Next/bitstring>>, Accu) ->
    decode0(Next, [<<14:5>>|Accu]);
decode0(<<"P", Next/bitstring>>, Accu) ->
    decode0(Next, [<<15:5>>|Accu]);
decode0(<<"Q", Next/bitstring>>, Accu) ->
    decode0(Next, [<<16:5>>|Accu]);
decode0(<<"R", Next/bitstring>>, Accu) ->
    decode0(Next, [<<17:5>>|Accu]);
decode0(<<"S", Next/bitstring>>, Accu) ->
    decode0(Next, [<<18:5>>|Accu]);
decode0(<<"T", Next/bitstring>>, Accu) ->
    decode0(Next, [<<19:5>>|Accu]);
decode0(<<"U", Next/bitstring>>, Accu) ->
    decode0(Next, [<<20:5>>|Accu]);
decode0(<<"V", Next/bitstring>>, Accu) ->
    decode0(Next, [<<21:5>>|Accu]);
decode0(<<"W", Next/bitstring>>, Accu) ->
    decode0(Next, [<<22:5>>|Accu]);
decode0(<<"X", Next/bitstring>>, Accu) ->
    decode0(Next, [<<23:5>>|Accu]);
decode0(<<"Y", Next/bitstring>>, Accu) ->
    decode0(Next, [<<24:5>>|Accu]);
decode0(<<"Z", Next/bitstring>>, Accu) ->
    decode0(Next, [<<25:5>>|Accu]);
decode0(<<"2", Next/bitstring>>, Accu) ->
    decode0(Next, [<<26:5>>|Accu]);
decode0(<<"3", Next/bitstring>>, Accu) ->
    decode0(Next, [<<27:5>>|Accu]);
decode0(<<"4", Next/bitstring>>, Accu) ->
    decode0(Next, [<<28:5>>|Accu]);
decode0(<<"5", Next/bitstring>>, Accu) ->
    decode0(Next, [<<29:5>>|Accu]);
decode0(<<"6", Next/bitstring>>, Accu) ->
    decode0(Next, [<<30:5>>|Accu]);
decode0(<<"7", Next/bitstring>>, Accu) ->
    decode0(Next, [<<31:5>>|Accu]);

decode0(<<"======", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode0(Next, [<<0:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<4:5>>|Accu]) ->
    decode0(Next, [<<1:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<8:5>>|Accu]) ->
    decode0(Next, [<<2:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<12:5>>|Accu]) ->
    decode0(Next, [<<3:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode0(Next, [<<4:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<20:5>>|Accu]) ->
    decode0(Next, [<<5:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<24:5>>|Accu]) ->
    decode0(Next, [<<6:3>>|Accu]);
decode0(<<"======", Next/bitstring>>, [<<28:5>>|Accu]) ->
    decode0(Next, [<<7:3>>|Accu]);

decode0(<<"====", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode0(Next, [<<0:1>>|Accu]);
decode0(<<"====", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode0(Next, [<<1:1>>|Accu]);

decode0(<<"===", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode0(Next, [<<0:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<2:5>>|Accu]) ->
    decode0(Next, [<<1:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<4:5>>|Accu]) ->
    decode0(Next, [<<2:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<6:5>>|Accu]) ->
    decode0(Next, [<<3:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<8:5>>|Accu]) ->
    decode0(Next, [<<4:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<10:5>>|Accu]) ->
    decode0(Next, [<<5:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<12:5>>|Accu]) ->
    decode0(Next, [<<6:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<14:5>>|Accu]) ->
    decode0(Next, [<<7:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode0(Next, [<<8:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<18:5>>|Accu]) ->
    decode0(Next, [<<9:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<20:5>>|Accu]) ->
    decode0(Next, [<<10:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<22:5>>|Accu]) ->
    decode0(Next, [<<11:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<24:5>>|Accu]) ->
    decode0(Next, [<<12:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<26:5>>|Accu]) ->
    decode0(Next, [<<13:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<28:5>>|Accu]) ->
    decode0(Next, [<<14:4>>|Accu]);
decode0(<<"===", Next/bitstring>>, [<<30:5>>|Accu]) ->
    decode0(Next, [<<15:4>>|Accu]);

decode0(<<"=", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode0(Next, [<<0:2>>|Accu]);
decode0(<<"=", Next/bitstring>>, [<<8:5>>|Accu]) ->
    decode0(Next, [<<1:2>>|Accu]);
decode0(<<"=", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode0(Next, [<<2:2>>|Accu]);
decode0(<<"=", Next/bitstring>>, [<<24:5>>|Accu]) ->
    decode0(Next, [<<3:2>>|Accu]).

