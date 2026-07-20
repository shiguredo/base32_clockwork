# base32:decode/2 のエラー型統一

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/change-unify-decode-error-type
- Polished: {YYYY-MM-DD}

## 目的

`base32:decode/2` のエラー型がフォーマット間で不統一であり、呼び出し側が統一的なエラーハンドリングを行えない。統一する。

## 優先度根拠

0002 の修正（crockford:decode のエラー返却化）と同時に対応しないと、`base32.erl:24` の `{ok, base32_crockford:decode(Data)}` が `{ok, {error, invalid}}` という二重ラップを引き起こす。

## 現状

- clockwork: `{error, invalid_size | invalid_format}`
- rfc4648: `{error, invalid_format}`
- crockford_check: `{error, invalid}`
- crockford: クラッシュ（`{error, _}` を返さない）

`base32.erl:24` の `{ok, base32_crockford:decode(Data)}` は、crockford:decode を `{ok, _} | {error, _}` 返却に修正すると二重ラップになる。

## 設計方針

全フォーマットで `{error, invalid_format}` に統一する。`base32.erl` の crockford 経路を `case` 分岐に変更する。

## 完了条件

- 全フォーマットのエラー型が `{error, invalid_format}` に統一される
- `base32:decode(crockford, Data)` がクラッシュせず `{ok, _} | {error, _}` を返す
- 既存のテストが全て通る

## 解決方法

- `base32.erl:24` を `base32_crockford:decode(Data)` の直接呼び出しに変更する
- 各モジュールのエラーアトムを `invalid_format` に統一する
- 0002 の修正と同時に行う
