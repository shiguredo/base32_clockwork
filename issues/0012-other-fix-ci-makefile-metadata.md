# CI・Makefile・メタデータの整備

- Priority: Low
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-ci-makefile-metadata
- Polished: {YYYY-MM-DD}

## 目的

CI ワークフロー・Makefile・メタデータに軽微な不備がある。整備する。

## 優先度根拠

機能的な問題はないが、再現性・一貫性・正確性の観点で改善が必要。

## 現状

1. **CI の GitHub Actions がコミットハッシュ固定されていない**（`.github/workflows/ci.yml:19,45,59`）: `actions/checkout@v4`、`shiguredo/github-actions/...@main` がタグ/ブランチ参照。
2. **CI の `paths-ignore` が push のみ**（`.github/workflows/ci.yml:4-8`）: `pull_request` にはないため、doc 変更のみの PR でも CI が走る。
3. **proper 依存がブランチ指定でバージョン未固定**（`rebar.config:39`）: `{branch, "master"}` は再現性の観点で望ましくない。
4. **Makefile の `compile` ターゲットが `xref` を実行する**（`Makefile:9`）: ターゲット名と実体が不一致。
5. **Makefile の `all` / `ci` に `efmt-check` がない**（`Makefile:3,26`）: ローカルで CI を再現できない。
6. **`base32.app.src` の description が不正確**（`src/base32.app.src:2`）: `"Clockwork Base32"` だが RFC 4648・Crockford も含む。
7. **README.md の著作権年が古い**（`README.md:43`）: `2021-2024` だが現在は 2026 年。
8. **rebar.config のコメントアウトされた dialyzer 設定**（`rebar.config:19,21-22,26`）: 意図が不明。
9. **`test/base32_crockford_test.erl:68` のコメントが日本語でない**: 規約「コメントは全て日本語」に違反。
10. **README.md:22 の decode 実行例が実際の返却値と異なる**: `<<"Wow, it really works!">>` だが実際は `{ok, <<"Wow, it really works!">>}`。
11. **ci.yml の ci ジョブに timeout-minutes がない**（`.github/workflows/ci.yml:40-46`）: デフォルト 360 分。dialyzer や proper がハングするとランナーを占有し続ける。

## 設計方針

- GitHub Actions をコミットハッシュ固定 + バージョンコメント形式にする
- proper 依存をタグまたはリビジョンで固定する
- Makefile のターゲット名と実体を一致させる
- メタデータを正確にする
- コメントアウトされた設定を削除するか理由を明記する

## 完了条件

- GitHub Actions がコミットハッシュ固定される
- Makefile のターゲット名と実体が一致する
- メタデータが正確になる
- 既存のテスト・CI が通る

## 解決方法

- 各ファイルの上記箇所を修正する
