# base32:decode/2 のエラー型統一

- Priority: Medium
- Created: 2026-07-20
- Completed: 2026-10-26
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
- crockford: クラッシュ (`{error, _}` を返さない。0002 の修正で `{error, invalid_format}` になる)

crockford の返却型と `base32.erl` の crockford 経路は 0002 で修正する。本 issue は残る clockwork と crockford_check のエラーアトムを統一する。

## 設計方針

エラー型は `invalid_format` の 1 種に統一する (対応時に 2 種統一から再変更)。

- `base32_crockford:decode_check/1` のチェックサム不一致の `{error, invalid}` を `{error, invalid_format}` に変更する
- `base32_crockford:decode_check/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更する
- `base32_clockwork:decode0/2` の `decode0(<<_:8>>, [])` 節の `{error, invalid_size}` を `{error, invalid_format}` に変更する
- `base32_clockwork:decode/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更する
- rfc4648 の戻り値の欠陥 (0005) は本 issue の範囲外。再検討後に対応する

## 完了条件

- 全フォーマットのエラー型が `{error, invalid_format}` に統一される
- 既存のテストが全て通る

## 解決方法

fuzzing (0002) の修正後に実施した。

1 回目の対応では「`invalid_size` と `invalid_format` の 2 種に統一する」判断で clockwork の `invalid_size` を維持して closed にしたが、`invalid_size` が発生するのは clockwork でシンボル 1 文字だけの入力のときのみであり、シンボル 1 文字 (5 ビット) は 1 バイトに満たない形式として不完全な入力であるため、特別扱いする実利が薄い。エラー型は `invalid_format` の 1 種に統一する判断に再変更した。

- `base32_crockford:decode_check/1` のチェックサム不一致の `{error, invalid}` を `{error, invalid_format}` に変更した
- `base32_crockford:decode_check/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更した (0002 の修正で空入力対応も実施済み)
- `base32_clockwork:decode0/2` の `decode0(<<_:8>>, [])` 節の `{error, invalid_size}` を `{error, invalid_format}` に変更した (シンボル 1 文字は 1 バイトに満たないため形式として不完全な入力として拒否する)
- `base32_clockwork:decode/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更した
- `test/base32_clockwork_test.erl` の `decode_error_test` の `invalid_size` 期待値を `invalid_format` に更新した
- `test/base32_crockford_test.erl` にチェックサム不一致のテスト (`decode_check_mismatch_test`) を追加した
- 拒否される入力は変わらず、エラーのアトムのみが変わる。`make ci` と `make fuzzing` (30 秒 / 144 万回、クラッシュ 0 件) を再実行して通過を確認した
- CHANGES.md の `## develop` に [CHANGE] エラー型の統一を追記した

注意:
- 0002 の修正後に実施すること
- `base32_crockford:decode/1` の返却型と `base32.erl` の crockford 経路は 0002 で修正するため、本 issue では変更しない
- rfc4648 (0005) の対応は本 issue の範囲外。設計方針の再検討後に対応する
