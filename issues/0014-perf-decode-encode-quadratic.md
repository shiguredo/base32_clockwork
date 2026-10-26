# decode・encode 経路の O(n^2) パフォーマンス問題

- Priority: Low
- Created: 2026-07-20
- Completed: 2026-10-26
- Model: qwen3.8-max-preview
- Branch: feature/refactor-performance
- Polished: {YYYY-MM-DD}
- Refreshed: 2026-10-26

## 目的

decode・encode 経路に O(n^2) のアルゴリズムが存在し、大量入力で実用的な問題になりうる。改善する。

## 優先度根拠

UUID（16 バイト → 26 文字）では問題ないが、1MB 入力では数十秒〜数分オーダーの差になりうる。現状で問題が報告されているわけではないため Low。

## 現状

1. **`base32_utils:rev_bits_list_to_binary0`**（`src/base32_utils.erl` の `rev_bits_list_to_binary0/2` の再帰節）: `<<Bits/bitstring, Accu/bitstring>>` が各ステップで Accu 全体をコピー。全体で O(n^2)。

2. **`base32_crockford:data_to_integer0`**（`src/base32_crockford.erl` の `data_to_integer0/3` の再帰節）: `(N bsl 5) bor Bits` が各ステップで bignum の全 limb をコピー+シフト。全体で O(n^2)。

3. **`base32_crockford:encode1`**（`src/base32_crockford.erl` の `encode1/3`）: `Value div 32` / `Value rem 32` が各ステップで bignum の除算。全体で O(n^2)。

4. **decode 経路のメモリ増幅**（全 decode モジュール）: 各文字ごとに 5-bit bitstring をヒープに確保。1MB 入力で約 100 倍のメモリ増幅。

5. **`base32_clockwork:decode0` の `bits_list_size` 再走査は解消済み**: 0001 対応 (79982cb) で `decode0/2` の完了節が全ビット連結後の `bit_size` でパディングを検証する方式に書き換えられ、再走査は発生しなくなった。ただし副産物として `base32_utils:bits_list_size/1` が呼び出し元のない dead code になっているため、本 issue で削除する。

## 設計方針

- `rev_bits_list_to_binary` を `list_to_bitstring(lists:reverse(List))` に置き換える（O(n)）
- `data_to_integer` を `binary:decode_unsigned/1` に置き換える（O(n)）
- `encode1` の bignum 除算をビットパターンマッチ方式に置き換える（O(n)）
- decode 経路で 8 文字（40 bit = 5 byte）ごとにまとめて binary に変換する

## 完了条件

- 1MB 入力の encode/decode が実用的な時間（数秒以内）で完了する
- 既存のテストが全て通る

## 解決方法

- `base32_utils` の bitstring リスト連結をやめ、5 ビットを整数に蓄積して 8 シンボル (40 bit) ごとに 5 byte へ確定する `push_symbol/2` と、末尾のパディングビットを検証して切り詰める `finalize_decode/1` にした。呼び出し元のない `bits_list_size/1` は削除した
- `base32_clockwork` と `base32_crockford` の decode をこの蓄積に切り替えた。clockwork のシンボル 1 文字の拒否は `decode/1` に残した
- `base32_rfc4648` はパディングが最後のシンボルを書き換えるため、8 シンボルちょうどは次のシンボルが来るまで確定しない。パディングなし入力が非バイトアラインの bitstring を返す挙動は維持した
- `base32_crockford` の encode は binary を 5 ビットずつ読み、integer は必要な桁数のビット列にして同じ経路でシンボル化する。チェックシンボルはバイトごとに 37 の剰余を取る
- 変更前の 992 件の入出力と一致した。1MB の encode / decode (encode_check / decode_check を含む) はいずれも 100ms 未満。EUnit 27 件、proper 7 プロパティ、dialyzer、xref が通る
