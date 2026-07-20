# base32_crockford:decode_check の入力検証欠落

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-crockford-decode-check-validation
- Polished: {YYYY-MM-DD}

## 目的

`base32_crockford:decode_check/1` に複数の入力検証欠落があり、特定の入力でクラッシュする。修正する。

## 優先度根拠

空 binary 入力でクラッシュし、ハイフン付き入力でクラッシュする。チェックシンボルの case-insensitive 比較もできていない。

## 現状

1. **空 binary でクラッシュ**（`src/base32_crockford.erl:124-125`）: `Size = (size(Data) - 1) * 8` が `-8` になり `badmatch`。

実測: `decode_check(<<>>)` → `error:{badmatch,<<>>}`

2. **ハイフンを考慮せずに分割する**（`src/base32_crockford.erl:123-131`）: 末尾ハイフンがチェックシンボルとして切り出され、データ部のデコードでクラッシュする。

実測: `decode_check(<<"CRW-">>)` → `error:{badmatch,<<102,28:7>>}`

3. **チェックシンボル比較が case-sensitive**（`src/base32_crockford.erl:128`）: `Check =:= Expectedcheck` で raw バイトを直接比較。小文字のチェックシンボルが拒否される。

実測: `decode_check(<<"CRw">>)` → `{error, invalid}`（`<<"CRW">>` は `{ok, <<"f">>}`）

## 設計方針

- 分割前に入力からハイフンを除去する
- ハイフン除去後に空 binary なら `{error, invalid_format}` を返す（0006 のエラーアトム統一に合わせる）
- チェックシンボル比較前に大文字に正規化する
- `decode(Data0)` の返却値を `case` で分岐し、`{error, _}` 時は `{error, invalid_format}` を返す（0002 の修正後に decode が `{ok, _} | {error, _}` を返すようになるため）

## 完了条件

- `decode_check(<<>>)` が `{error, invalid_format}` を返す
- `decode_check(<<"-">>)` が `{error, invalid_format}` を返す（ハイフン除去後に空になる）
- `decode_check(<<"CRW-">>)` が `{ok, <<"f">>}` を返す
- `decode_check(<<"CRw">>)` が `{ok, <<"f">>}` を返す
- 既存のテストが全て通る

## 解決方法

- 分割前に `binary:replace(Data, <<"-">>, <<>>, [global])` でハイフンを除去する
- ハイフン除去結果が空 binary なら `{error, invalid_format}` を返す（空入力チェックはハイフン除去**後**に行うこと。`decode_check(<<"-">>)` がガードを通過してクラッシュするのを防ぐため）
- `decode(Data0)` の返却値を `case` で分岐する
- `Expectedcheck` を大文字に正規化してから比較する
- `is_binary(Data)` ガードを追加する

注意: 0002・0006 の修正後に実施すること（decode の返却型とエラーアトムに依存する）
