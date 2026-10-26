-module(base32_utils).

-export([new_decode_acc/0, push_symbol/2, finalize_decode/1]).

%% decode の途中状態。
%% Acc は未確定ビットを上位から詰めた整数、BitCount はそのビット数、
%% Out は確定済みの 5 byte binary を新しいものから積んだリスト。
-type decode_acc() :: {non_neg_integer(), non_neg_integer(), [binary()]}.


-spec new_decode_acc() -> decode_acc().
new_decode_acc() ->
    {0, 0, []}.


%% 5 ビットシンボルを 1 つ追加する。
%% 8 シンボル (40 bit = 5 byte) 溜まった時点で binary として確定し、
%% 文字ごとの bitstring 確保と、確定済みビット列の再コピーを避ける。
-spec push_symbol(0..31, decode_acc()) -> decode_acc().
push_symbol(Value, {Acc, 35, Out}) ->
    Combined = (Acc bsl 5) bor Value,
    {0, 0, [<<Combined:40>> | Out]};
push_symbol(Value, {Acc, BitCount, Out}) ->
    {(Acc bsl 5) bor Value, BitCount + 5, Out}.


%% 未確定ビットの末尾 (ビット数を 8 で割った余り) はパディングである。
%% すべて 0 なら切り捨て、確定済みの binary と連結して返す。1 ビットでも
%% 立っていれば、正しいエンコード結果ではないので invalid_format を返す。
-spec finalize_decode(decode_acc()) -> {ok, binary()} | {error, invalid_format}.
finalize_decode({_Acc, 0, Out}) ->
    {ok, iolist_to_binary(lists:reverse(Out))};
finalize_decode({Acc, BitCount, Out}) ->
    Padding = BitCount rem 8,
    case Acc band ((1 bsl Padding) - 1) of
        0 ->
            {ok, iolist_to_binary(lists:reverse([tail_binary(Acc bsr Padding,
                                                             BitCount - Padding) | Out]))};
        _ ->
            {error, invalid_format}
    end.


%% パディングを除いた残りビットを、上位を 0 で埋めてバイト列にする。
%% DataBits は 0, 8, 16, 24, 32 のいずれか。
-spec tail_binary(non_neg_integer(), non_neg_integer()) -> binary().
tail_binary(_Value, 0) ->
    <<>>;
tail_binary(Value, DataBits) ->
    Size = DataBits div 8,
    Tail = binary:encode_unsigned(Value),
    %% unit 指定は fuzzer の辞書生成が解釈できないため、ビット数でゼロ埋めする
    PadBits = (Size - byte_size(Tail)) * 8,
    <<0:PadBits, Tail/binary>>.
