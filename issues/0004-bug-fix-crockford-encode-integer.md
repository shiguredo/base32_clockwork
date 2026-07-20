# base32_crockford:encode integer 経路の欠陥

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-crockford-encode-integer
- Polished: {YYYY-MM-DD}

## 目的

`base32_crockford:encode/1` の integer 経路の桁数計算が根本的に間違っており、V >= 32 のほぼ全ての値で誤った出力を返す。`encode(0)` も空 binary を返す。修正する。

## 優先度根拠

`encode(32)` が `<<"0">>` を返す（正しくは `<<"10">>`）。デコードすると 0 になり、データが破壊される。

## 現状

1. **Count 計算の誤り**（`src/base32_crockford.erl:57-63`）: `BaseCount = Value div 32` は商であり base32 桁数ではない。

実測:
- `encode(32)` → `<<"0">>`（正しくは `<<"10">>`）
- `encode(96)` → `<<"030">>`（正しくは `<<"30">>`）
- `encode(255)` → `<<"0000007Z">>`（正しくは `<<"7Z">>`）
- `encode(1024)` → 32 桁のゼロ埋め（正しくは `<<"100">>`）

binary 経路の `ceil(bit_size / 5)` は正しい。integer 経路だけが間違っている。

2. **encode(0) が空 binary**（`src/base32_crockford.erl:57-63`）: `Value = 0` のとき `Count = 0` となり `encode1(0, 0, [])` は `[]` を返す。

実測: `encode(0)` → `<<>>`（正しくは `<<"0">>`）

3. **is_integer ガードがない**（`src/base32_crockford.erl:57`, `:43`）: 第 1 節の `when is_binary(Data)` にマッチしない任意の項が integer 節に落ちる。

実測: `encode(hello)` → `error:badarith`

## 設計方針

- Count 計算を「Value を 32 で繰り返し除算して桁数を求める」ロジックに修正する
- `encode(0)` が `<<"0">>` を返すようにする
- `is_integer(Value), Value >= 0` ガードを追加する

## 完了条件

- `encode(32)` が `<<"10">>` を返す
- `encode(0)` が `<<"0">>` を返す
- `encode(hello)` が `function_clause` でクラッシュする
- `encode_check/1` の integer 経路も同時に修正される
- 既存のテストが全て通る

## 解決方法

- `encode0(Value, Accu)` の Count 計算を再帰的な桁数計算に変更する
- `encode0(Value, Accu) when is_integer(Value), Value >= 0 ->` にガードを追加する
- `encode_check(Value) when is_integer(Value), Value >= 0 ->` にガードを追加する
