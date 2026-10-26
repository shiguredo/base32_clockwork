# 単体テストの追加（エラーパス・境界値・case-insensitive・エイリアス）

- Priority: Medium
- Created: 2026-07-20
- Completed: 2026-10-26
- Model: qwen3.8-max-preview
- Branch: develop
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

- 全 facade 経路 (4 フォーマット) のテストが存在する
- エラーパスのテストが既存テストで網羅されている
- case-insensitive / エイリアスと空 binary は PBT (0009) で検証する
- `-import` がテストから削除されている
- 冗長な `ok` が削除されている
- `make test` と `make proper` が通る

## 解決方法

- テストの追加方針を PBT 前提に変更した。case-insensitive / エイリアスと空 binary は EUnit で網羅せず 0009 の PBT で検証する
- EUnit に不足していた facade (crockford_check / clockwork) のテストを追加する
- decode_check のチェックサム不一致と encode_check の integer 入力は 0001〜0006 の対応で既に追加済み
- rfc4648 の decode が invalid_format を返すのは catch-all 節 1 経路のみで、既存の 1 件がその経路をカバーしているため追加はしない
- テスト 4 ファイルの `-import` を削除して完全修飾呼び出しに統一する
- 冗長な `ok` を削除する
- efmt / elint / EUnit (27 件) / dialyzer / xref / `make proper` (7 プロパティ) が通ることを確認する
