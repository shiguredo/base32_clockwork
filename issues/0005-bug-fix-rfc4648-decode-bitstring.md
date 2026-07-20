# base32_rfc4648:decode が非バイトアラインの bitstring を返す

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-rfc4648-decode-bitstring
- Polished: {YYYY-MM-DD}

## 目的

`base32_rfc4648:decode/1` がパディングなしの不正長入力を受理し、非バイトアラインの bitstring を返す。`-spec` は `binary()`（バイトアライン）を宣言しており、契約違反。修正する。

## 優先度根拠

下流コードが `binary()` を前提に `byte_size/1` や `binary_to_list/1` を呼ぶと `badarg` でクラッシュする。

## 現状

`src/base32_rfc4648.erl:166-167` の基底節 `decode0(<<>>, Accu) -> {ok, rev_bits_list_to_binary(Accu)}` にバイトアラインの検証がない。

実測:
- `decode(<<"A">>)` → `{ok, <<0:5>>}`（5-bit bitstring、`is_binary/1` = false）
- `decode(<<"AAAAAAAAA">>)` → `{ok, <<0:45>>}`（45-bit bitstring）

RFC 4648 ではパディングなしの入力長は 2, 4, 5, 7, 8 (mod 8) のみ有効。

## 設計方針

`decode0(<<>>, Accu)` で全ビット数が 8 の倍数であることを検証し、そうでなければ `{error, invalid_format}` を返す。

## 完了条件

- `decode(<<"A">>)` が `{error, invalid_format}` を返す
- 正当なパディング付き入力は引き続き `{ok, binary()}` を返す
- 既存のテストが全て通る

## 解決方法

- `decode0(<<>>, Accu)` で `bit_size(Result) rem 8 =/= 0` の場合に `{error, invalid_format}` を返す
