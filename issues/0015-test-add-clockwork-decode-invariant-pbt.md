# clockwork decode の受理・拒否の不変条件を検証する PBT を追加する

- Priority: Medium
- Created: 2026-09-24
- Completed: {YYYY-MM-DD}
- Model: DeepSeek V4.1 Flash
- Branch: feature/add-clockwork-decode-invariant-pbt
- Polished: {YYYY-MM-DD}

## 目的

`base32_clockwork:decode/1` の終端処理（パディングビットの検証と切り詰め）の不変条件を PBT で検証できていない。非 canonical な入力を含む任意のシンボル列に対して、受理・拒否の条件と返却値のバイトアラインをプロパティとして固定する。

## 優先度根拠

現行の PBT は canonical な encode 出力の roundtrip のみで、パディングビット非ゼロ拒否の経路やパディング長 5〜7 に構造的に到達しない。終端処理を壊す変更を入れても PBT では検出できない。

## 現状

- `test/prop_base32_clockwork.erl` の `prop_base32_clockwork_encode_decode/1` は encode → decode の roundtrip のみ
- canonical な encode 出力のパディング長は 0〜4 に限られるため、パディング長 5〜7 とパディングビット非ゼロ拒否の経路は PBT で実行されない
- 単体テスト（`test/base32_clockwork_test.erl` の `decode_padding_test/0`）が代表例を固定しているが、任意のシンボル列に対する不変条件は未検証

## 設計方針

- シンボル表（0〜31 とエイリアス）から任意長（0〜100 程度）のシンボル列を生成するジェネレータを追加する
- 生成したシンボル列を 5 bit ずつ連結した参照ビット列をオラクルとして、次を検証する
  - 末尾のパディングビットがすべてゼロなら `{ok, Binary}`（1 文字入力のみ既存どおり `{error, invalid_size}`）を返し、`Binary` は `is_binary/1` かつ `byte_size(Binary) =:= (5 * N) div 8` を満たす
  - パディングビットが 1 つでも非ゼロなら `{error, invalid_format}` を返す
- オラクルが実装の写経にならないよう、シンボル表から参照ビット列を独立に構築する

## 完了条件

- 追加したプロパティが `rebar3 as test proper` で通る
- 既存の PBT / 単体テストが全て通る

## 解決方法

- `test/prop_base32_clockwork.erl` に任意シンボル列の不変条件を検証するプロパティを追加する
