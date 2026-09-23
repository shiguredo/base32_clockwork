# base32_crockford:decode0 のエラーハンドリング欠落

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/change-crockford-decode-error-handling
- Polished: 2026-09-23

## 目的

`base32_crockford:decode0` に catch-all 節がなく、不正文字入力で `function_clause` クラッシュする。また、パディングビット非ゼロの入力で `badmatch` クラッシュする。`base32:decode/2` の spec `{ok, binary()} | {error, atom()}` に違反するため修正する。修正に伴い `base32_crockford:decode/1` の返却型を `{ok, binary()} | {error, invalid_format}` に変更する（モジュール公開 API の後方互換なし変更）。

## 優先度根拠

`base32:decode(crockford, <<"*">>)` がクラッシュし、呼び出し元にそのまま伝播する。facade の spec 契約違反であり、本番環境でプロセスが落ちる経路。

## 現状

1. `base32_crockford` の `decode0/2` の最終節がハイフン節のみで catch-all がない。`U`, `*`, `~`, `$`, `=` 等の不正文字や非 ASCII バイト入力で `function_clause` クラッシュ。

実測: `base32_crockford:decode(<<"*">>)` → `error:function_clause`

2. `decode0(<<>>, Accu)` 節の `<<Decoded1:DataSize/bitstring, 0:PaddingSize>> = Decoded0` がパディングビット非ゼロで `badmatch` クラッシュ。有効文字のみで構成される入力でも発生する。

実測: `base32_crockford:decode(<<"CSQPYR">>)` → `error:{badmatch,<<102,111,111,24:6>>}`

3. `base32_crockford:decode0/2` の各節は `Next/bitstring` を使うが、`base32_clockwork:decode0/2` は `Next/binary` を使っており不統一。

## 設計方針

- `decode/1` の戻り値を `{ok, binary()} | {error, invalid_format}` に変更する
- `decode0/2` に catch-all 節を追加し `{error, invalid_format}` を返す
- パディングビット検証を `case` に書き換え、非ゼロ時に `{error, invalid_format}` を返す
- `base32.erl` の `decode(crockford, Data)` 節を `base32_crockford:decode(Data)` の直接呼び出しに変更する（二重ラップ回避。本 issue に含めることで 0006 と分離して実施できる）
- `decode_check/1` の `decode(Data0)` 呼び出しを `case` で受け、`{error, _}` の場合は `{error, invalid_format}` を返す（返却型変更への追随。ハイフン除去・空入力検証などの入力検証は本 issue の範囲外）
- `decode0/2` の各節の `Next/bitstring` を `Next/binary` に変更する

## 完了条件

- 不正文字入力で `{error, invalid_format}` を返す
- パディングビット非ゼロの入力で `{error, invalid_format}` を返す
- `base32:decode(crockford, Data)` がクラッシュせず `{ok, binary()} | {error, invalid_format}` を返す
- `decode_check/1` が返却型変更に追随し、`decode_check_test` を含む既存のテストが全て通る

## 解決方法

- `decode0` の末尾に `decode0(_, _) -> {error, invalid_format}` を追加する
- パディングビット検証を `case Decoded0 of <<Decoded1:DataSize/bitstring, 0:PaddingSize>> -> ...; _ -> {error, invalid_format} end` に変更する
- `decode/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更し、`decode0/2` の結果を `{ok, Decoded}` に包む（エラーはそのまま返す）
- `base32.erl` の `decode(crockford, Data)` 節を `base32_crockford:decode(Data)` の直接呼び出しに変更する
- `decode_check/1` の `decode(Data0)` 呼び出しを `case` で受け、`{error, _}` の場合は `{error, invalid_format}` を返す
- `decode_check/1` の spec を `{ok, binary()} | {error, invalid | invalid_format}` に変更する（チェックサム不一致の `invalid` は 0006 で `invalid_format` に統一する）
- `decode0/2` の各節の `Next/bitstring` を `Next/binary` に変更する
- `test/base32_test.erl` の crockford 経路、`test/base32_crockford_test.erl` の `decode_nocheck_test` / `decode_hyphen_test` / `other1_test` / `other2_test` のアサーションを `{ok, _}` マッチに更新する
- `CHANGES.md` の `## develop` に [CHANGE] `base32_crockford:decode/1` の返却型変更と、[FIX] 不正入力でのクラッシュ修正を追記する

注意:
- 返却型変更はモジュール公開 API の後方互換なし変更のため、ブランチは `feature/change-` を使う
- 0006 は本 issue の修正後に実施する。`base32.erl` の crockford 経路と `base32_crockford:decode/1` の返却型は本 issue で対応するため、0006 と同一 PR にする必要はない
- ハイフン除去・空入力検証・チェックシンボルの大文字小文字対応は別 issue の範囲とする
