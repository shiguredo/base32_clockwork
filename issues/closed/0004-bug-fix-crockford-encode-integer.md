# base32_crockford:encode integer 経路の欠陥

- Priority: High
- Created: 2026-07-20
- Completed: 2026-10-26
- Model: qwen3.8-max-preview
- Branch: feature/fix-crockford-encode-integer
- Polished: 2026-09-23

## 目的

`base32_crockford:encode/1` の integer 経路の桁数計算が根本的に間違っており、V >= 32 のほぼ全ての値で誤った出力を返す。`encode(0)` も空 binary を返す。修正する。

## 優先度根拠

`encode(32)` が `<<"0">>` を返す（正しくは `<<"10">>`）。デコード結果が元の値と一致しなくなり、データが破壊される。

## 現状

1. **Count 計算の誤り**（`base32_crockford:encode0/2` の integer 節）: `BaseCount = Value div 32` は商であり base32 桁数ではない。

実測:
- `encode(32)` → `<<"0">>`（正しくは `<<"10">>`）
- `encode(96)` → `<<"030">>`（正しくは `<<"30">>`）
- `encode(255)` → `<<"0000007Z">>`（正しくは `<<"7Z">>`）
- `encode(1024)` → 32 桁のゼロ埋め（正しくは `<<"100">>`）

binary 経路の `ceil(bit_size / 5)` は正しい。integer 経路だけが間違っている。

2. **encode(0) が空 binary**（`base32_crockford:encode0/2` の integer 節）: `Value = 0` のとき `Count = 0` となり `encode1(0, 0, [])` は `[]` を返す。

実測: `encode(0)` → `<<>>`（正しくは `<<"0">>`）

3. **is_integer ガードがない**（`base32_crockford:encode0/2` の integer 節と `encode_check/1` の integer 節）: 第 1 節の `when is_binary(Data)` にマッチしない任意の項が integer 節に落ちる。

実測: `encode(hello)` → `error:badarith`

## 設計方針

- Count 計算を「Value を 32 で繰り返し除算して桁数を求める」ロジックに修正する
- `encode(0)` が `<<"0">>` を返すようにする
- `encode0(Value, Accu) when is_integer(Value), Value >= 0 ->` にガードを追加する（`encode_check/1` の integer 節の存否は別 issue で判断するため、本 issue では変更しない）

## 完了条件

- `encode(32)` が `<<"10">>` を返す
- `encode(0)` が `<<"0">>` を返す
- `encode(hello)` が `function_clause` でクラッシュする
- `encode0/2` の integer 節の共有により `encode_check/1` の integer 経路の出力桁が正しくなることを確認する（binary 経路の出力は変わらない）
- 既存のテストが全て通る

## 解決方法

- `encode0/2` の integer 節の Count 計算を再帰的な桁数計算に変更する（binary 節は変更しない）
- `encode0(Value, Accu) when is_integer(Value), Value >= 0 ->` にガードを追加する
- `CHANGES.md` の `## develop` に [FIX] `encode/1` の integer 経路の桁数計算の修正を追記する

注意:
- `encode/1` の integer 経路は整数の canonical な Base32 文字列を生成するものであり、`decode/1` が同じ整数値を復元することは保証しない
- `encode_check/1` の integer 節の存否と `decode_check/1` のラウンドトリップの設計判断は保留中であり、本 issue では行わない
- `encode1/3` の除算を置き換える性能改善 (0014) と同じ関数群に触れるため、本 issue を先に実施する

## 解決方法

PBT (PropEr) で潰した。`test/prop_base32_crockford.erl` に「`encode(V)` の出力シンボル列を 5 ビットごとに再び整数に戻すと V と一致する」プロパティ (`prop_base32_crockford_encode_integer/1`、10 万回) を追加し、`encode/1` の integer 経路は `decode/1` がバイト境界で切詰めるため binary レベルの roundtrip は成立しない。そのためシンボル値の連結で検証した。PBT は最小反例 `32` を検出し、`encode(32)` が `<<"0">>` を返す現状の欠陥を実証した。

- `encode0/2` の integer 節の Count 計算を再帰的な桁数計算 (`digit_count/1`) に変更した
- `encode(0)` が `<<"0">>` を返すようにした (0 は 1 桁として扱う)
- `encode0(Value, Accu) when is_integer(Value), Value >= 0` にガードを追加し、integer 以外の入力は `function_clause` でクラッシュする
- `test/base32_crockford_test.erl` に `encode_integer_test` / `encode_invalid_input_test` / `encode_check_integer_test` を追加した。`encode_check/1` の integer 経路は `encode0/2` を共有するため、出力桁が正しい canonical な表現 (`encode_check(42)` は `<<"1A5">>`) になることを確認した
- `make ci` と `make fuzzing` (30 秒、クラッシュ 0 件) を再実行して通過を確認した
