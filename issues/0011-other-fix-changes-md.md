# CHANGES.md の修正

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-changes-md
- Polished: {YYYY-MM-DD}

## 目的

`CHANGES.md` の `## develop` セクションに規約違反と不整合がある。修正する。

## 優先度根拠

AGENTS.md の変更履歴規約に違反しており、リリース時の混乱を招く。

## 現状

1. **種別順序違反**（`CHANGES.md:7-18`）: 現在の順序は CHANGE → CHANGE → CHANGE → ADD → UPDATE → FIX。規約は UPDATE → ADD → CHANGE → FIX の順。
2. **エントリフォーマット違反**（`CHANGES.md:13`）: `- [ADD] PBT を追加` であり「〜する」で終わっていない。
3. **rebar3 バージョン不一致**（`CHANGES.md:16`）: `3.25.1` と記載されているが、実際は `3.27.0`。
4. **import 削除の記載とテストコードの不整合**（`CHANGES.md:18`）: 「import を利用しないようにする」と記載済みだが、テスト 4 ファイルに `-import` が残っている。
5. **リリース日フォーマット**（`CHANGES.md:23`）: `日付: 2023-05-02` ではなく `**リリース日**: 2023-05-02` が規約のフォーマット。

## 設計方針

- 種別順序を UPDATE → ADD → CHANGE → FIX に並べ替える
- エントリフォーマットを修正する
- rebar3 バージョンを `3.27.0` に修正する
- import 削除の記載を「src/ の import を削除する」に限定するか、テストからも import を削除する（0010 と連携）
- リリース日フォーマットを修正する

## 完了条件

- `## develop` セクションの種別順序が規約に準拠する
- 全エントリが「〜する」で終わる
- rebar3 バージョンが実物と一致する
- 記載内容とコードが整合する

## 解決方法

- `CHANGES.md` の `## develop` セクションを並べ替え・修正する
