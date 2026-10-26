-module(base32_crockford).

-export([encode/1, encode_check/1, decode/1, decode_check/1]).


%% 負整数は encode0/2 の非負条件で function_clause になるため non_neg_integer() に限定する
-spec encode(binary() | non_neg_integer()) -> binary().
encode(Value) ->
    Encoded = encode0(Value, []),
    list_to_binary(Encoded).


-spec encode_check(binary() | non_neg_integer()) -> binary().
encode_check(Data) when is_binary(Data) ->
    Check = symbol(mod37(Data)),
    Encoded = encode0(Data, [Check]),
    list_to_binary(Encoded);
encode_check(Value) when is_integer(Value), Value >= 0 ->
    Check = symbol(mod37(binary:encode_unsigned(Value))),
    Encoded = encode0(Value, [Check]),
    list_to_binary(Encoded).


%% シンボルは先頭から順に作るが、リストは末尾追加を避けるため逆順に積む。
%% 呼び出し側の lists:reverse/2 で正順に戻し、Accu (チェックシンボル) を末尾に付ける。
encode0(Data, Accu) when is_binary(Data) ->
    lists:reverse(symbols_from_bits(Data, []), Accu);
encode0(Value, Accu) when is_integer(Value), Value >= 0 ->
    lists:reverse(symbols_from_integer(Value), Accu).


%% バイト列を 5 ビットずつ読む。端数はシンボルの上位に寄せ、下位を 0 で埋める。
symbols_from_bits(<<Bits:5, Next/bits>>, Acc) ->
    symbols_from_bits(Next, [symbol(Bits) | Acc]);
symbols_from_bits(<<Bits:4>>, Acc) ->
    [symbol(Bits bsl 1) | Acc];
symbols_from_bits(<<Bits:3>>, Acc) ->
    [symbol(Bits bsl 2) | Acc];
symbols_from_bits(<<Bits:2>>, Acc) ->
    [symbol(Bits bsl 3) | Acc];
symbols_from_bits(<<Bits:1>>, Acc) ->
    [symbol(Bits bsl 4) | Acc];
symbols_from_bits(<<>>, Acc) ->
    Acc.


%% 0 は 1 桁の "0" とする。それ以外は必要な 5 ビット桁だけを使い、先頭の 0 は付けない。
symbols_from_integer(0) ->
    [symbol(0)];
symbols_from_integer(Value) ->
    TotalBits = symbol_count(Value) * 5,
    <<Bits:TotalBits/bits>> = <<Value:TotalBits>>,
    symbols_from_bits(Bits, []).


%% Value を表すのに必要なシンボル数 (32 進の桁数)。
symbol_count(Value) ->
    (bit_length(Value) + 4) div 5.


%% 最上位ビットの位置 (1 始まり)。
bit_length(Value) ->
    Bin = binary:encode_unsigned(Value),
    (byte_size(Bin) - 1) * 8 + byte_bit_length(binary:first(Bin)).


byte_bit_length(Byte) ->
    byte_bit_length(Byte, 0).


byte_bit_length(0, Length) ->
    Length;
byte_bit_length(Byte, Length) ->
    byte_bit_length(Byte bsr 1, Length + 1).


%% バイト列を符号なし整数とみなした 37 の剰余。
%% 整数を一度に作ると大きい入力で system_limit になるため、1 バイトずつ畳む。
mod37(Data) ->
    mod37(Data, 0).


mod37(<<>>, Acc) ->
    Acc;
mod37(<<Byte, Rest/binary>>, Acc) ->
    mod37(Rest, (Acc * 256 + Byte) rem 37).


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
    $Z;
symbol(32) ->
    $*;
symbol(33) ->
    $~;
symbol(34) ->
    $$;
symbol(35) ->
    $=;
symbol(36) ->
    $U.


-spec decode(binary()) -> {ok, binary()} | {error, invalid_format}.
decode(Data) when is_binary(Data) ->
    decode0(Data, base32_utils:new_decode_acc()).


-spec decode_check(binary()) -> {ok, binary()} | {error, invalid_format}.
decode_check(Data) when is_binary(Data) ->
    %% 分割前にハイフンを除去する (ハイフンは decode0/2 と同様に無視する)
    case binary:replace(Data, <<"-">>, <<>>, [global]) of
        <<>> ->
            %% チェックシンボルを含まない入力は無効な入力として拒否する
            {error, invalid_format};
        Hyphenless ->
            decode_check0(Hyphenless)
    end.


%% ハイフンを除去した入力の最後の文字をチェックシンボルとして扱い、
%% デコード結果から計算したチェックシンボルと比較する
-spec decode_check0(binary()) -> {ok, binary()} | {error, invalid_format}.
decode_check0(Hyphenless) ->
    Size = (size(Hyphenless) - 1) * 8,
    <<Data0:Size/bitstring, Expectedcheck:8>> = Hyphenless,
    case decode(Data0) of
        {ok, Decoded} ->
            match_check_symbol(Decoded, Expectedcheck);
        {error, invalid_format} ->
            {error, invalid_format}
    end.


%% デコード結果から計算したチェックシンボルが入力のチェックシンボルと一致するかを判定する
-spec match_check_symbol(binary(), byte()) -> {ok, binary()} | {error, invalid_format}.
match_check_symbol(Decoded, Expectedcheck) ->
    case symbol(mod37(Decoded)) =:= normalize_check_symbol(Expectedcheck) of
        true ->
            {ok, Decoded};
        false ->
            {error, invalid_format}
    end.


%% チェックシンボルの比較は大文字小文字を区別しないため、ASCII の a から z のみ大文字化する
-spec normalize_check_symbol(byte()) -> byte().
normalize_check_symbol(C) when C >= $a, C =< $z ->
    C - 32;
normalize_check_symbol(C) ->
    C.


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
decode0(<<"-", Next/binary>>, Acc) ->
    decode0(Next, Acc);
decode0(_, _) ->
    {error, invalid_format}.
