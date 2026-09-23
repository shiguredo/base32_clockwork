# encode_check(integer) と decode_check のラウンドトリップが構造的に破綻している

- Priority: Medium
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-encode-check-integer-roundtrip
- Polished: {YYYY-MM-DD}

## 目的

`base32_crockford:encode_check/1` の integer 経路と `decode_check/1` のラウンドトリップが構造的に破綻している。設計判断を行い、修正する。

## 優先度根拠

integer 経路の encode_check は check symbol を元の整数値から計算するが、decode_check は decode 後の binary の整数解釈から計算する。5bit×N → 8bit アラインへの切詰めで元の整数値が保存されないため、check symbol が一致しない。

## 現状

`encode_check(42)` は check symbol を `check_symbol(42) = symbol(42 rem 37) = symbol(5) = $5` から計算する。`decode_check` 側ではデータ部をデコードして binary を得て、`check_symbol(data_to_integer(Decoded, false))` で検証する。この二つは異なる値（42 vs デコード結果の整数解釈）になる。

Crockford の仕様は入力を non-negative integer と定義し、decode も integer を返す。しかし本ライブラリの decode は binary を返すため、整数値が保存されない。

## 設計方針

以下のいずれかを選択する:

- (a) `encode_check/1` の integer 経路を廃止し、`is_binary` ガードのみにする
- (b) `decode_check/1` が integer を返す経路を別途設ける（ただし入力から整数由来か判定不能）

0004 の修正と同時に対応する。

## 完了条件

- encode_check → decode_check のラウンドトリップが全経路で成立する、または integer 経路が明示的に廃止される
- 既存のテストが全て通る

## 解決方法

- 設計判断 (a) の場合: `encode_check(Value) ->` 節を削除し、`encode_check(Data) when is_binary(Data) ->` のみにする
- 設計判断 (b) の場合: `decode_check_integer/1` を別途追加する

## pending とした理由

- `encode_check/1` の integer 経路と `decode_check/1` のラウンドトリップは、5bit エンコードの切詰めで整数値が保存されない構造的な問題であり、(a) integer 経路の廃止（公開 API の後方互換なし削除）か (b) 別 API の追加（入力から整数由来かを判定できないため、ラウンドトリップの定義自体の再設計）のいずれかを選択する必要がある。
- どちらを選ぶかで `encode_check/1` の spec の扱いと 0007 の内容が変わり、後方互換への影響も大きいため、方針が固まるまで保留する。
- 保留中は 0004 を本 issue に依存させず、`encode/1` の integer 経路に限定して実施する。
