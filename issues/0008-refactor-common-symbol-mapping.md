# symbol/1 と decode0 マッピングの共通化

- Priority: Low
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/refactor-common-symbol-mapping
- Polished: {YYYY-MM-DD}

## 目的

`base32_clockwork` と `base32_crockford` で重複している `symbol/1` と `decode0` のシンボルマッピングを `base32_utils` に共通化する。

## 優先度根拠

機能的な問題はないが、64 個の同一パターンマッチ節が 2 モジュールに存在し、片方だけ修正して不整合が起きるリスクがある。

## 現状

1. `src/base32_clockwork.erl:25-56` と `src/base32_crockford.erl:78-109` の `symbol/1`（0〜31）が完全に同一。crockford は 32〜36 を追加しているだけ。

2. `src/base32_clockwork.erl:79-198` と `src/base32_crockford.erl:145-264` の `decode0` シンボルマッピング（大文字・小文字両対応）がほぼ同一。差異は終端処理と hyphen 節の有無のみ。

## 設計方針

- `base32_utils` に `encode_symbol(0..31) -> char()` と `decode_symbol(char()) -> {ok, 0..31} | error` を追加する（ハイフン処理は crockford 側の `decode0` で `decode_symbol` 呼び出し前に行う）
- `base32_utils` に `finalize_decode([bitstring()]) -> {ok, binary()} | {error, invalid_format}` を追加し、両モジュールの終端節（全ビット連結 + パディングビット検証 + 切り詰め）を共通化する
- 各モジュールの `symbol/1` と `decode0` から共通関数を呼び出す形に変更する
- crockford の 32〜36（チェックシンボル）と hyphen 節は各モジュール側で維持する

## 完了条件

- 重複コードが解消される（symbol マッピング + 終端節）
- 既存のテストが全て通る
- 挙動が変わらない

## 解決方法

- `base32_utils` に共通関数を追加し、両モジュールから呼び出す

注意: 0001・0002 の修正後に実施すること（修正対象コードが重複するため）
