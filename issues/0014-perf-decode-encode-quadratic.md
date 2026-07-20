# decode・encode 経路の O(n^2) パフォーマンス問題

- Priority: Low
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/refactor-performance
- Polished: {YYYY-MM-DD}

## 目的

decode・encode 経路に O(n^2) のアルゴリズムが存在し、大量入力で実用的な問題になりうる。改善する。

## 優先度根拠

UUID（16 バイト → 26 文字）では問題ないが、1MB 入力では数十秒〜数分オーダーの差になりうる。現状で問題が報告されているわけではないため Low。

## 現状

1. **`base32_utils:rev_bits_list_to_binary0`**（`src/base32_utils.erl:12-13`）: `<<Bits/bitstring, Accu/bitstring>>` が各ステップで Accu 全体をコピー。全体で O(n^2)。

2. **`base32_crockford:data_to_integer0`**（`src/base32_crockford.erl:28-29`）: `(N bsl 5) bor Bits` が各ステップで bignum の全 limb をコピー+シフト。全体で O(n^2)。

3. **`base32_crockford:encode1`**（`src/base32_crockford.erl:68-71`）: `Value div 32` / `Value rem 32` が各ステップで bignum の除算。全体で O(n^2)。

4. **decode 経路のメモリ増幅**（全 decode モジュール）: 各文字ごとに 5-bit bitstring をヒープに確保。1MB 入力で約 100 倍のメモリ増幅。

5. **`base32_clockwork:decode0` の `bits_list_size` 再走査**（`src/base32_clockwork.erl:66-67`）: decode 完了時に Accu リスト全体を再走査。カウンタで保持すれば不要。

## 設計方針

- `rev_bits_list_to_binary` を `list_to_bitstring(lists:reverse(List))` に置き換える（O(n)）
- `data_to_integer` を `binary:decode_unsigned/1` に置き換える（O(n)）
- `encode1` の bignum 除算をビットパターンマッチ方式に置き換える（O(n)）
- decode 経路で 8 文字（40 bit = 5 byte）ごとにまとめて binary に変換する

## 完了条件

- 1MB 入力の encode/decode が実用的な時間（数秒以内）で完了する
- 既存のテストが全て通る

## 解決方法

- 各関数を上記の方針で書き換える
- ベンチマークで改善を確認する
