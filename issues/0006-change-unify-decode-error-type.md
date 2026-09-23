# base32:decode/2 のエラー型統一

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/change-unify-decode-error-type
- Polished: 2026-09-23

## 目的

`base32:decode/2` のエラー型がフォーマット間で不統一であり、呼び出し側が統一的なエラーハンドリングを行えない。統一する。

## 優先度根拠

clockwork は `invalid_size` と `invalid_format` を返し分け、crockford_check は `invalid` を返すため、呼び出し側がエラー型を一様に扱えない。

## 現状

- clockwork: `{error, invalid_size | invalid_format}`
- rfc4648: `{error, invalid_format}`
- crockford_check: `{error, invalid}`
- crockford: クラッシュ（`{error, _}` を返さない。0002 の修正で `{error, invalid_format}` になる）

crockford の返却型と `base32.erl` の crockford 経路は 0002 で修正する。本 issue は残る clockwork と crockford_check のエラーアトムを統一する。

## 設計方針

全フォーマットで `{error, invalid_format}` に統一する。

- `base32_clockwork:decode0/2` の `decode0(<<_:8>>, [])` 節の `invalid_size` を `invalid_format` に変更する
- `base32_crockford:decode_check/1` のチェックサム不一致の `{error, invalid}` を `{error, invalid_format}` に変更する
- 変更に伴い `decode/1` と `decode_check/1` の spec、該当テストの期待値を更新する

## 完了条件

- 全フォーマットのエラー型が `{error, invalid_format}` に統一される
- `base32:decode(crockford, Data)` がクラッシュせず `{ok, _} | {error, _}` を返す（0002 の修正で達成済み）
- 既存のテストが全て通る

## 解決方法

- `base32_clockwork:decode0/2` の `decode0(<<_:8>>, [])` 節の `{error, invalid_size}` を `{error, invalid_format}` に変更する
- `base32_clockwork:decode/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更する
- `test/base32_clockwork_test.erl` の `decode_error_test` の `invalid_size` 期待値を `invalid_format` に更新する
- `base32_crockford:decode_check/1` のチェックサム不一致の `{error, invalid}` を `{error, invalid_format}` に変更する
- `base32_crockford:decode_check/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更する
- `CHANGES.md` の `## develop` に [CHANGE] エラー型の統一を追記する

注意:
- 0002 の修正後に実施すること
- `base32_crockford:decode/1` の返却型と `base32.erl` の crockford 経路は 0002 で修正するため、本 issue では変更しない
