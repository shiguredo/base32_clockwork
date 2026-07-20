# base32_crockford:decode0 のエラーハンドリング欠落

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-crockford-decode-error-handling
- Polished: {YYYY-MM-DD}

## 目的

`base32_crockford:decode0` に catch-all 節がなく、不正文字入力で `function_clause` クラッシュする。また、パディングビット非ゼロの入力で `badmatch` クラッシュする。`base32:decode/2` の spec `{ok, binary()} | {error, atom()}` に違反するため修正する。

## 優先度根拠

`base32:decode(crockford, <<"*">>)` がクラッシュし、呼び出し元にそのまま伝播する。facade の spec 契約違反であり、本番環境でプロセスが落ちる経路。

## 現状

1. `src/base32_crockford.erl:265-266` の最終節がハイフンのみ。`U`, `*`, `~`, `$`, `=` 等の不正文字や非 ASCII バイト入力で `function_clause` クラッシュ。

実測: `base32_crockford:decode(<<"*">>)` → `error:function_clause`

2. `src/base32_crockford.erl:142` の `<<Decoded1:DataSize/bitstring, 0:PaddingSize>> = Decoded0` がパディングビット非ゼロで `badmatch` クラッシュ。有効文字のみで構成される入力でも発生する。

実測: `base32_crockford:decode(<<"CSQPYR">>)` → `error:{badmatch,<<102,111,111,24:6>>}`

3. `src/base32_crockford.erl:145` で `Next/bitstring` を使うが、`base32_clockwork.erl:79` は `Next/binary`。不統一。

## 設計方針

- `decode/1` の戻り値を `{ok, binary()} | {error, invalid_format}` に変更する
- `decode0` に catch-all 節を追加し `{error, invalid_format}` を返す
- パディングビット検証を `case` に書き換え、非ゼロ時にエラーを返す
- `Next/bitstring` を `Next/binary` に統一する

## 完了条件

- 不正文字入力で `{error, invalid_format}` を返す
- パディングビット非ゼロの入力で `{error, invalid_format}` を返す
- `base32:decode(crockford, Data)` がクラッシュしない
- 既存のテストが全て通る

## 解決方法

- `decode0` の末尾に `decode0(_, _) -> {error, invalid_format}` を追加する
- パディングビット検証を `case Decoded0 of <<Decoded1:DataSize/bitstring, 0:PaddingSize>> -> ...; _ -> {error, invalid_format} end` に変更する
- `decode/1` の spec を `{ok, binary()} | {error, invalid_format}` に変更する
- `Next/bitstring` を `Next/binary` に変更する
- `base32.erl:24` のラッパーを `case` 分岐に変更する（0006 と連携）

注意:
- **0006 と同一 PR で実施すること**。分離すると `{ok, {error, _}}` の二重ラップ中間状態が発生し、呼び出し側が `{ok, Decoded} = decode(crockford, Data)` で `Decoded` を binary として使うと `badarg` でクラッシュする
- 返却型変更に伴い、`test/base32_test.erl:21`、`test/base32_crockford_test.erl:31-36,49-56,63,72` の既存アサーションを `{ok, _}` マッチに更新する
