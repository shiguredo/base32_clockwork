# spec 修正と base32_format() の export

- Priority: Medium
- Created: 2026-07-20
- Completed: 2026-10-26
- Model: qwen3.8-max-preview
- Branch: develop
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

- `base32.erl` に `-export_type([base32_format/0])` を追加する
- `base32:encode/2` の spec を `(base32_format(), binary() | non_neg_integer()) -> binary()` に修正する (crockford 系のみ整数を受け付けるため、負整数は除外する)
- `base32_crockford:encode/1` と `encode_check/1` の spec を `binary() | non_neg_integer()` に修正する (負整数は `encode0/2` の非負条件で function_clause になるため `integer()` より狭い型にする)
- `base32:decode/2` の spec の `{error, atom()}` を `{error, invalid_format}` に修正する (完了条件の「全公開関数に正確な spec」に含める)
- `base32_utils` の `rev_bits_list_to_binary/1` と `bits_list_size/1` に spec を追加する (連結結果はバイト境界に整っていない bitstring() になり得るため返値は `bitstring()`)
- efmt / elint / EUnit (25 件) / dialyzer / xref が通ることを確認する
