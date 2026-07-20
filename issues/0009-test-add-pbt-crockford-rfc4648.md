# PBT の追加（crockford・rfc4648）と既存 PBT の改善

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/add-pbt-crockford-rfc4648
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

- `test/prop_base32_crockford.erl` と `test/prop_base32_rfc4648.erl` を新規作成する
- `test/prop_base32_clockwork.erl` の range と性質を追加する
