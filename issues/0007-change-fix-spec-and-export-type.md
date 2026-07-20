# spec 修正と base32_format() の export

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/change-fix-spec-and-export-type
- Polished: {YYYY-MM-DD}

## 目的

型仕様の不備を修正し、公開 API の型を正しく export する。

## 優先度根拠

Dialyzer の検出能力を毀損し、利用者が spec で型を参照できない。

## 現状

1. `src/base32.erl:5` の `base32_format()` に `-export_type` がない
2. `src/base32.erl:8` の `encode/2` の spec が `binary()` のみだが、crockford は integer も受け付ける
3. `src/base32_crockford.erl:38` の `encode_check/1` の spec が `binary()` のみだが、integer も受け付ける
4. `src/base32_utils.erl:6,16` の公開関数に `-spec` がない

## 設計方針

- `-export_type([base32_format/0]).` を追加する
- `encode/2` の spec を `(base32_format(), binary() | integer()) -> binary()` に修正する
- `encode_check/1` の spec を `(binary() | integer()) -> binary()` に修正する
- `base32_utils` の公開関数に spec を追加する

## 完了条件

- `base32_format()` が外部から参照できる
- 全公開関数に正確な spec がある
- `rebar3 dialyzer` が通る

## 解決方法

- 各ファイルに上記の spec 修正を追加する
