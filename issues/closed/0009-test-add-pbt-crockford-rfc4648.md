# PBT の追加（crockford・rfc4648）と既存 PBT の改善

- Priority: Medium
- Created: 2026-07-20
- Completed: 2026-10-26
- Model: qwen3.8-max-preview
- Branch: develop
- Polished: {YYYY-MM-DD}

## 目的

PBT が clockwork のみで、crockford と rfc4648 に存在しない。また、既存の clockwork PBT も空 binary と case-insensitive / エイリアスの性質をカバーしていない。追加・改善する。

## 優先度根拠

crockford は integer エンコードや check symbol など複雑な経路があり、PBT による検証が特に重要。

## 現状

- `test/prop_base32_clockwork.erl` のみ存在。crockford・rfc4648 には PBT がない
- `range(1, 100)` のため空 binary（N=0）が生成されない
- roundtrip のみで、case-insensitive / O・I・L エイリアスの性質を検証していない

## 設計方針

- `prop_base32_crockford.erl` を追加: encode/decode roundtrip、encode_check/decode_check roundtrip
- `prop_base32_rfc4648.erl` を追加: encode/decode roundtrip
- 既存 PBT の `range(1, 100)` を `range(0, 100)` に変更する
- case-insensitive / エイリアスの性質を PBT で追加する

## 完了条件

- 3 モジュール全てに roundtrip PBT が存在する
- 空 binary の roundtrip が PBT でカバーされる
- `make proper` が通る

## 解決方法

- `test/prop_base32_crockford.erl` に binary の encode/decode roundtrip、encode_check/decode_check roundtrip、case-insensitive / O・I・L エイリアスの性質を追加する
- `test/prop_base32_rfc4648.erl` を新規作成し、encode/decode roundtrip を検証する
- `test/prop_base32_clockwork.erl` の range を (0, 100) に変更して空 binary の roundtrip をカバーし、case-insensitive / O・I・L エイリアスの性質を追加する
- エイリアスの性質は canonical なエンコード出力の A から Z を小文字化する変換と、0 を O、1 を I、1 を L に置換する変換を oneof で選ぶ 1 プロパティで検証する
- efmt / elint / EUnit (27 件) / dialyzer / xref / `make proper` (7 プロパティ) が通ることを確認する
