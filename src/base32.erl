-module(base32).

-export([encode/2, encode_to_string/2]).

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
    N = Size div 5 + (case Size rem 5 of
                          0 -> 0;
                          _ -> 1
                      end),
    case 40 - (Size rem 40) of
        40 ->
            RevEncoded = encode_rfc4648_to_string0(Data, N, []),
            {ok, lists:reverse(RevEncoded)};
        32 ->
            RevEncoded = encode_rfc4648_to_string0(Data, N, []),
            {ok, lists:reverse([$=, $=, $=, $=, $=, $=|RevEncoded])};
        24 ->
            RevEncoded = encode_rfc4648_to_string0(Data, N, []),
            {ok, lists:reverse([$=, $=, $=, $=|RevEncoded])};
        16 ->
            RevEncoded = encode_rfc4648_to_string0(Data, N, []),
            {ok, lists:reverse([$=, $=, $=|RevEncoded])};
        8 ->
            RevEncoded = encode_rfc4648_to_string0(Data, N, []),
            {ok, lists:reverse([$=|RevEncoded])};
        _ ->
            error(badarg)
    end.

encode_rfc4648_to_string0(<<>>, 0, Accu) ->
    Accu;
encode_rfc4648_to_string0(Bin, 0, _) ->
    {error, {halt, Bin}};

encode_rfc4648_to_string0(<<0:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$A|Accu]);
encode_rfc4648_to_string0(<<1:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$B|Accu]);
encode_rfc4648_to_string0(<<2:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$C|Accu]);
encode_rfc4648_to_string0(<<3:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$D|Accu]);
encode_rfc4648_to_string0(<<4:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$E|Accu]);
encode_rfc4648_to_string0(<<5:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$F|Accu]);
encode_rfc4648_to_string0(<<6:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$G|Accu]);
encode_rfc4648_to_string0(<<7:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$H|Accu]);
encode_rfc4648_to_string0(<<8:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$I|Accu]);
encode_rfc4648_to_string0(<<9:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$J|Accu]);
encode_rfc4648_to_string0(<<10:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$K|Accu]);
encode_rfc4648_to_string0(<<11:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$L|Accu]);
encode_rfc4648_to_string0(<<12:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$M|Accu]);
encode_rfc4648_to_string0(<<13:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$N|Accu]);
encode_rfc4648_to_string0(<<14:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$O|Accu]);
encode_rfc4648_to_string0(<<15:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$P|Accu]);
encode_rfc4648_to_string0(<<16:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Q|Accu]);
encode_rfc4648_to_string0(<<17:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$R|Accu]);
encode_rfc4648_to_string0(<<18:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$S|Accu]);
encode_rfc4648_to_string0(<<19:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$T|Accu]);
encode_rfc4648_to_string0(<<20:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$U|Accu]);
encode_rfc4648_to_string0(<<21:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$V|Accu]);
encode_rfc4648_to_string0(<<22:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$W|Accu]);
encode_rfc4648_to_string0(<<23:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$X|Accu]);
encode_rfc4648_to_string0(<<24:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Y|Accu]);
encode_rfc4648_to_string0(<<25:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Z|Accu]);
encode_rfc4648_to_string0(<<26:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$2|Accu]);
encode_rfc4648_to_string0(<<27:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$3|Accu]);
encode_rfc4648_to_string0(<<28:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$4|Accu]);
encode_rfc4648_to_string0(<<29:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$5|Accu]);
encode_rfc4648_to_string0(<<30:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$6|Accu]);
encode_rfc4648_to_string0(<<31:5, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$7|Accu]);

encode_rfc4648_to_string0(<<0:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$A|Accu]);
encode_rfc4648_to_string0(<<1:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$C|Accu]);
encode_rfc4648_to_string0(<<2:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$E|Accu]);
encode_rfc4648_to_string0(<<3:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$G|Accu]);
encode_rfc4648_to_string0(<<4:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$I|Accu]);
encode_rfc4648_to_string0(<<5:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$K|Accu]);
encode_rfc4648_to_string0(<<6:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$M|Accu]);
encode_rfc4648_to_string0(<<7:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$O|Accu]);
encode_rfc4648_to_string0(<<8:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Q|Accu]);
encode_rfc4648_to_string0(<<9:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$S|Accu]);
encode_rfc4648_to_string0(<<10:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$U|Accu]);
encode_rfc4648_to_string0(<<11:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$W|Accu]);
encode_rfc4648_to_string0(<<12:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Y|Accu]);
encode_rfc4648_to_string0(<<13:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$2|Accu]);
encode_rfc4648_to_string0(<<14:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$4|Accu]);
encode_rfc4648_to_string0(<<15:4, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$6|Accu]);

encode_rfc4648_to_string0(<<0:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$A|Accu]);
encode_rfc4648_to_string0(<<1:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$E|Accu]);
encode_rfc4648_to_string0(<<2:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$I|Accu]);
encode_rfc4648_to_string0(<<3:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$M|Accu]);
encode_rfc4648_to_string0(<<4:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Q|Accu]);
encode_rfc4648_to_string0(<<5:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$U|Accu]);
encode_rfc4648_to_string0(<<6:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Y|Accu]);
encode_rfc4648_to_string0(<<7:3, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$4|Accu]);

encode_rfc4648_to_string0(<<0:2, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$A|Accu]);
encode_rfc4648_to_string0(<<1:2, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$I|Accu]);
encode_rfc4648_to_string0(<<2:2, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Q|Accu]);
encode_rfc4648_to_string0(<<3:2, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Y|Accu]);

encode_rfc4648_to_string0(<<0:1, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$A|Accu]);
encode_rfc4648_to_string0(<<1:1, Next/bitstring>>, N, Accu) ->
    encode_rfc4648_to_string0(Next, N-1, [$Q|Accu]);

encode_rfc4648_to_string0(_, _, _) ->
    {error, {fatal, rfc4648}}.


