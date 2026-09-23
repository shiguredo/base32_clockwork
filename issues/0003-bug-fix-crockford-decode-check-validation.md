# base32_crockford:decode_check の入力検証欠落

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-crockford-decode-check-validation
- Polished: 2026-09-23

## 目的

`base32_crockford:decode_check/1` に複数の入力検証欠落があり、特定の入力でクラッシュする。修正する。

## 優先度根拠

空 binary 入力と末尾ハイフン付き入力でクラッシュまたは誤った結果になる。チェックシンボルの case-insensitive 比較もできていない。

## 現状

1. **空 binary でクラッシュ**（`base32_crockford:decode_check/1` の `Size = (size(Data) - 1) * 8` と `<<Data0:Size/bitstring, Expectedcheck:8>> = Data`）: `Size` が `-8` になり `badmatch`。

実測: `decode_check(<<>>)` → `error:{badmatch,<<>>}`

2. **ハイフンを考慮せずに分割する**（`base32_crockford:decode_check/1` の入力分割処理）: 末尾ハイフンがチェックシンボルとして切り出され、データ部のデコードでクラッシュする。

実測: `decode_check(<<"CRW-">>)` → `error:{badmatch,<<102,28:7>>}`

3. **チェックシンボル比較が case-sensitive**（`base32_crockford:decode_check/1` の `Check =:= Expectedcheck`）: raw バイトを直接比較しており、小文字のチェックシンボルが拒否される。

実測: `decode_check(<<"CRw">>)` → `{error, invalid}`（`<<"CRW">>` は `{ok, <<"f">>}`）

## 設計方針

- 分割前に入力からハイフンを除去する（バイト単位の `binary:replace/4` を使う）
- ハイフン除去後に空 binary なら `{error, invalid_format}` を返す
- チェックシンボル比較前に ASCII の `a`〜`z` のみを大文字に正規化する（`string:uppercase/1` は `Expectedcheck` が整数のために `badarg` となり、バイナリに包んでも非 ASCII バイトで `badarg` になるため使わない）
- `decode(Data0)` の返却値の `case` 分岐と spec 更新は 0002 で実施するため、本 issue では変更しない

## 完了条件

- `decode_check(<<>>)` が `{error, invalid_format}` を返す
- `decode_check(<<"-">>)` が `{error, invalid_format}` を返す（ハイフン除去後に空になる）
- `decode_check(<<"CRW-">>)` が `{ok, <<"f">>}` を返す
- `decode_check(<<"CRw">>)` が `{ok, <<"f">>}` を返す
- `decode_check(<<"CR", 227>>)` がクラッシュせず `{error, invalid}` を返す（0006 実施後は `{error, invalid_format}`）
- 既存のテストが全て通る

## 解決方法

- 分割前に `binary:replace(Data, <<"-">>, <<>>, [global])` でハイフンを除去する
- ハイフン除去結果が空 binary なら `{error, invalid_format}` を返す（空入力チェックはハイフン除去**後**に行うこと。`decode_check(<<"-">>)` がガードを通過してクラッシュするのを防ぐため）
- ASCII の `a`〜`z` のみを大文字化してから比較する（例: `C >= $a, C =< $z` のとき `C - 32`）
- `is_binary(Data)` ガードを追加する
- `CHANGES.md` の `## develop` に [FIX] 入力検証の欠落によるクラッシュと小文字チェックシンボル拒否の修正を追記する
- `decode(Data0)` の `case` 分岐は 0002 で実施するため変更しない

注意:
- 0002 の修正後に実施すること（`decode/1` の返却型に依存する）。0002 実施後は現状 2 の実測値も `{error, invalid_format}` に変わる
- 0006 の前後どちらでも成立する。0006 実施後は現状 3 の実測値が `{error, invalid_format}` に変わる
