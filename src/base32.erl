-module(base32).

-export([encode/2, encode_to_string/2, decode/2, decode_to_string/2]).

-import(base32_utils, [rev_bits_list_to_binary/1]).

-type ascii_string() :: [1..255].
-type ascii_binary() :: binary().
-type base32_data() :: ascii_string() | ascii_binary().
-type base32_format() :: rfc4648.

-spec encode(base32_format(), base32_data()) ->
    {ok, ascii_binary()} | {error, atom()}.
encode(Format, Data) ->
    case encode_to_string(Format, Data) of
        {ok, Encoded} ->
            {ok, list_to_binary(Encoded)};
        Error ->
            Error
    end.

-spec encode_to_string(base32_format(), base32_data()) ->
    {ok, ascii_string()} | {error, atom()}.
encode_to_string(Format, Data) when is_list(Data) ->
    encode_to_string(Format, list_to_binary(Data));
encode_to_string(rfc4648, Data) ->
    encode_rfc4648_to_string(Data);
encode_to_string(Format, _) ->
    {error, {invalid_format, Format}}.

encode_rfc4648_to_string(Data) ->
    Size = bit_size(Data),
    case 40 - (Size rem 40) of
        40 ->
            RevEncoded = encode_rfc4648_to_string0(Data, []),
            {ok, lists:reverse(RevEncoded)};
        32 ->
            RevEncoded = encode_rfc4648_to_string0(Data, []),
            {ok, lists:reverse([$=, $=, $=, $=, $=, $=|RevEncoded])};
        24 ->
            RevEncoded = encode_rfc4648_to_string0(Data, []),
            {ok, lists:reverse([$=, $=, $=, $=|RevEncoded])};
        16 ->
            RevEncoded = encode_rfc4648_to_string0(Data, []),
            {ok, lists:reverse([$=, $=, $=|RevEncoded])};
        8 ->
            RevEncoded = encode_rfc4648_to_string0(Data, []),
            {ok, lists:reverse([$=|RevEncoded])};
        _ ->
            error(badarg)
    end.

encode_rfc4648_to_string0(<<>>, Accu) ->
    Accu;

encode_rfc4648_to_string0(<<0:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$A|Accu]);
encode_rfc4648_to_string0(<<1:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$B|Accu]);
encode_rfc4648_to_string0(<<2:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$C|Accu]);
encode_rfc4648_to_string0(<<3:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$D|Accu]);
encode_rfc4648_to_string0(<<4:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$E|Accu]);
encode_rfc4648_to_string0(<<5:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$F|Accu]);
encode_rfc4648_to_string0(<<6:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$G|Accu]);
encode_rfc4648_to_string0(<<7:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$H|Accu]);
encode_rfc4648_to_string0(<<8:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$I|Accu]);
encode_rfc4648_to_string0(<<9:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$J|Accu]);
encode_rfc4648_to_string0(<<10:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$K|Accu]);
encode_rfc4648_to_string0(<<11:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$L|Accu]);
encode_rfc4648_to_string0(<<12:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$M|Accu]);
encode_rfc4648_to_string0(<<13:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$N|Accu]);
encode_rfc4648_to_string0(<<14:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$O|Accu]);
encode_rfc4648_to_string0(<<15:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$P|Accu]);
encode_rfc4648_to_string0(<<16:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$Q|Accu]);
encode_rfc4648_to_string0(<<17:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$R|Accu]);
encode_rfc4648_to_string0(<<18:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$S|Accu]);
encode_rfc4648_to_string0(<<19:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$T|Accu]);
encode_rfc4648_to_string0(<<20:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$U|Accu]);
encode_rfc4648_to_string0(<<21:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$V|Accu]);
encode_rfc4648_to_string0(<<22:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$W|Accu]);
encode_rfc4648_to_string0(<<23:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$X|Accu]);
encode_rfc4648_to_string0(<<24:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$Y|Accu]);
encode_rfc4648_to_string0(<<25:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$Z|Accu]);
encode_rfc4648_to_string0(<<26:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$2|Accu]);
encode_rfc4648_to_string0(<<27:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$3|Accu]);
encode_rfc4648_to_string0(<<28:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$4|Accu]);
encode_rfc4648_to_string0(<<29:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$5|Accu]);
encode_rfc4648_to_string0(<<30:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$6|Accu]);
encode_rfc4648_to_string0(<<31:5, Next/bitstring>>, Accu) ->
    encode_rfc4648_to_string0(Next, [$7|Accu]);

encode_rfc4648_to_string0(<<0:4>>, Accu) ->
    [$A|Accu];
encode_rfc4648_to_string0(<<1:4>>, Accu) ->
    [$C|Accu];
encode_rfc4648_to_string0(<<2:4>>, Accu) ->
    [$E|Accu];
encode_rfc4648_to_string0(<<3:4>>, Accu) ->
    [$G|Accu];
encode_rfc4648_to_string0(<<4:4>>, Accu) ->
    [$I|Accu];
encode_rfc4648_to_string0(<<5:4>>, Accu) ->
    [$K|Accu];
encode_rfc4648_to_string0(<<6:4>>, Accu) ->
    [$M|Accu];
encode_rfc4648_to_string0(<<7:4>>, Accu) ->
    [$O|Accu];
encode_rfc4648_to_string0(<<8:4>>, Accu) ->
    [$Q|Accu];
encode_rfc4648_to_string0(<<9:4>>, Accu) ->
    [$S|Accu];
encode_rfc4648_to_string0(<<10:4>>, Accu) ->
    [$U|Accu];
encode_rfc4648_to_string0(<<11:4>>, Accu) ->
    [$W|Accu];
encode_rfc4648_to_string0(<<12:4>>, Accu) ->
    [$Y|Accu];
encode_rfc4648_to_string0(<<13:4>>, Accu) ->
    [$2|Accu];
encode_rfc4648_to_string0(<<14:4>>, Accu) ->
    [$4|Accu];
encode_rfc4648_to_string0(<<15:4>>, Accu) ->
    [$6|Accu];

encode_rfc4648_to_string0(<<0:3>>, Accu) ->
    [$A|Accu];
encode_rfc4648_to_string0(<<1:3>>, Accu) ->
    [$E|Accu];
encode_rfc4648_to_string0(<<2:3>>, Accu) ->
    [$I|Accu];
encode_rfc4648_to_string0(<<3:3>>, Accu) ->
    [$M|Accu];
encode_rfc4648_to_string0(<<4:3>>, Accu) ->
    [$Q|Accu];
encode_rfc4648_to_string0(<<5:3>>, Accu) ->
    [$U|Accu];
encode_rfc4648_to_string0(<<6:3>>, Accu) ->
    [$Y|Accu];
encode_rfc4648_to_string0(<<7:3>>, Accu) ->
    [$4|Accu];

encode_rfc4648_to_string0(<<0:2>>, Accu) ->
    [$A|Accu];
encode_rfc4648_to_string0(<<1:2>>, Accu) ->
    [$I|Accu];
encode_rfc4648_to_string0(<<2:2>>, Accu) ->
    [$Q|Accu];
encode_rfc4648_to_string0(<<3:2>>, Accu) ->
    [$Y|Accu];

encode_rfc4648_to_string0(<<0:1>>, Accu) ->
    [$A|Accu];
encode_rfc4648_to_string0(<<1:1>>, Accu) ->
    [$Q|Accu];

encode_rfc4648_to_string0(_, _) ->
    {error, {fatal, rfc4648}}.

-spec decode(base32_format(), base32_data()) ->
    {ok, ascii_binary()} | {error, atom()}.
decode(Format, Data) when is_list(Data) ->
    decode(Format, list_to_binary(Data));
decode(rfc4648, Data) ->
    {ok, decode_rfc4648_0(Data, [])};
decode(Format, _) ->
    {error, {invalid_format, Format}}.

-spec decode_to_string(base32_format(), base32_data()) ->
    {ok, ascii_string()} | {error, atom()}.
decode_to_string(Format, Data) ->
    case decode(Format, Data) of
        {ok, Decoded} ->
            {ok, binary_to_list(Decoded)};
        Error ->
            Error
    end.

decode_rfc4648_0(<<>>, Accu) ->
    rev_bits_list_to_binary(Accu);
decode_rfc4648_0(<<"A", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<0:5>>|Accu]);
decode_rfc4648_0(<<"B", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<1:5>>|Accu]);
decode_rfc4648_0(<<"C", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<2:5>>|Accu]);
decode_rfc4648_0(<<"D", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<3:5>>|Accu]);
decode_rfc4648_0(<<"E", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<4:5>>|Accu]);
decode_rfc4648_0(<<"F", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<5:5>>|Accu]);
decode_rfc4648_0(<<"G", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<6:5>>|Accu]);
decode_rfc4648_0(<<"H", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<7:5>>|Accu]);
decode_rfc4648_0(<<"I", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<8:5>>|Accu]);
decode_rfc4648_0(<<"J", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<9:5>>|Accu]);
decode_rfc4648_0(<<"K", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<10:5>>|Accu]);
decode_rfc4648_0(<<"L", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<11:5>>|Accu]);
decode_rfc4648_0(<<"M", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<12:5>>|Accu]);
decode_rfc4648_0(<<"N", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<13:5>>|Accu]);
decode_rfc4648_0(<<"O", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<14:5>>|Accu]);
decode_rfc4648_0(<<"P", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<15:5>>|Accu]);
decode_rfc4648_0(<<"Q", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<16:5>>|Accu]);
decode_rfc4648_0(<<"R", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<17:5>>|Accu]);
decode_rfc4648_0(<<"S", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<18:5>>|Accu]);
decode_rfc4648_0(<<"T", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<19:5>>|Accu]);
decode_rfc4648_0(<<"U", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<20:5>>|Accu]);
decode_rfc4648_0(<<"V", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<21:5>>|Accu]);
decode_rfc4648_0(<<"W", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<22:5>>|Accu]);
decode_rfc4648_0(<<"X", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<23:5>>|Accu]);
decode_rfc4648_0(<<"Y", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<24:5>>|Accu]);
decode_rfc4648_0(<<"Z", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<25:5>>|Accu]);
decode_rfc4648_0(<<"2", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<26:5>>|Accu]);
decode_rfc4648_0(<<"3", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<27:5>>|Accu]);
decode_rfc4648_0(<<"4", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<28:5>>|Accu]);
decode_rfc4648_0(<<"5", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<29:5>>|Accu]);
decode_rfc4648_0(<<"6", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<30:5>>|Accu]);
decode_rfc4648_0(<<"7", Next/bitstring>>, Accu) ->
    decode_rfc4648_0(Next, [<<31:5>>|Accu]);

decode_rfc4648_0(<<"======", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<0:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<4:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<1:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<8:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<2:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<12:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<3:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<4:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<20:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<5:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<24:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<6:3>>|Accu]);
decode_rfc4648_0(<<"======", Next/bitstring>>, [<<28:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<7:3>>|Accu]);

decode_rfc4648_0(<<"====", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<0:1>>|Accu]);
decode_rfc4648_0(<<"====", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<1:1>>|Accu]);

decode_rfc4648_0(<<"===", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<0:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<2:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<1:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<4:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<2:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<6:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<3:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<8:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<4:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<10:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<5:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<12:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<6:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<14:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<7:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<8:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<18:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<9:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<20:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<10:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<22:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<11:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<24:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<12:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<26:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<13:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<28:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<14:4>>|Accu]);
decode_rfc4648_0(<<"===", Next/bitstring>>, [<<30:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<15:4>>|Accu]);

decode_rfc4648_0(<<"=", Next/bitstring>>, [<<0:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<0:2>>|Accu]);
decode_rfc4648_0(<<"=", Next/bitstring>>, [<<8:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<1:2>>|Accu]);
decode_rfc4648_0(<<"=", Next/bitstring>>, [<<16:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<2:2>>|Accu]);
decode_rfc4648_0(<<"=", Next/bitstring>>, [<<24:5>>|Accu]) ->
    decode_rfc4648_0(Next, [<<3:2>>|Accu]);

decode_rfc4648_0(_, _) ->
    {error, {fatal, rfc4648}}.

