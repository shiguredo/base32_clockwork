# 単体テストの追加（エラーパス・境界値・case-insensitive・エイリアス）

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/add-unit-tests
- Polished: {YYYY-MM-DD}

## 目的

エラーパス・境界値・case-insensitive・エイリアスの単体テストが全体的に不足している。追加する。

## 優先度根拠

クラッシュ経路・エラーパスのテストが欠落しており、リグレッションを検出できない。

## 現状

1. `test/base32_test.erl` に `crockford_check` と `clockwork` の facade テストがない
2. `test/base32_crockford_test.erl` に `decode_check` のエラーケース（チェックサム不一致）のテストがない
3. `test/base32_crockford_test.erl` に `encode_check` の integer 入力のテストがない
4. clockwork decode の case-insensitive / O・I・L エイリアスのテストがない
5. rfc4648 のエラーケーステストが 1 件のみ
6. 空 binary の encode/decode テストが crockford と rfc4648 にない
7. テスト 4 ファイルに `-import` が残っている（CHANGES.md の記載と不整合）
8. `test/base32_crockford_test.erl:64,73` の末尾の冗長な `ok` がある

## 設計方針

- 各モジュールのエラーパス・境界値テストを追加する
- case-insensitive / エイリアスのテストを追加する
- `-import` を削除して完全修飾呼び出しに統一する
- 冗長な `ok` を削除する

## 完了条件

- 全 facade 経路（4 フォーマット）のテストが存在する
- 全エラーパスのテストが存在する
- case-insensitive / エイリアスのテストが存在する
- `-import` がテストから削除されている
- `make test` が通る

## 解決方法

- 各テストファイルに上記のテストケースを追加する
- `-import` を削除して `base32:encode(...)` 形式に統一する
