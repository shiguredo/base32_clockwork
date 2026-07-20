# base32_clockwork:decode0 のパディング除去ロジックの欠陥

- Priority: High
- Created: 2026-07-20
- Completed: {YYYY-MM-DD}
- Model: qwen3.8-max-preview
- Branch: feature/fix-clockwork-decode-padding
- Polished: {YYYY-MM-DD}

## 目的

`base32_clockwork:decode0` のパディング除去ロジックに構造的な欠陥があり、特定の入力長でクラッシュする。仕様の公式例がデコード不能であり、修正する。

## 優先度根拠

仕様の公式例 `CR0`（3 文字）がクラッシュするのは仕様違反。Sora の UUID エンコード（26 文字）は安全だが、27 文字入力でクラッシュする。

## 現状

`src/base32_clockwork.erl:68-74` で `BodySize = 5 - PaddingSize` を計算しているが、`PaddingSize`（= `Size rem 8`）が 5 を超える場合 `BodySize` が負になり `badmatch` でクラッシュする。

クラッシュする入力長: N mod 8 ∈ {3, 6}（3, 6, 11, 14, 19, 22, 27, 30, ... 文字）

実測:
- `decode(<<"CR0">>)` → `error:{badmatch,<<0:5>>}`（仕様の公式例）
- `decode(<<"CSQPYR">>)` → `error:{badmatch,<<24:5>>}`
- `decode(<<"CSQPYRK1E8C">>)` → クラッシュ（11 文字）

また、パディングビット非ゼロを暗黙的に許容する（`src/base32_clockwork.erl:73` の `_/bitstring`）。`decode(<<"C1">>)` はパディングビット `01` が非ゼロだが `{ok, <<"`">>}` を返す。crockford は badmatch、rfc4648 は error で拒否しており、同一ライブラリ内で検証の厳密さが不一致。

さらに、1 文字の不正入力（例: `decode(<<"U">>)`）が `{error, invalid_size}` を返すが、`U` はサイズではなくフォーマットの問題（`src/base32_clockwork.erl:77`）。

## 設計方針

`base32_crockford:decode0` と同様に、全ビットを連結してから末尾パディングを除去する方式に変更する。パディングビットが非ゼロの場合は `{error, invalid_format}` を返す。

## 完了条件

- `decode(<<"CR0">>)` が `{ok, <<"f">>}` を返す
- N mod 8 ∈ {3, 6} の任意の入力長でクラッシュしない
- パディングビット非ゼロの入力で `{error, invalid_format}` を返す
- 既存のテストが全て通る

## 解決方法

- `decode0(<<>>, Accu)` のパディング除去ロジックを、全ビット連結後に末尾を切り詰める方式に変更する
- パディングビットのゼロ検証を追加する
- 3 文字・6 文字・11 文字等のクラッシュ経路のテストを追加する

注意: 1 文字入力のエラー種別（`invalid_size` → `invalid_format`）の変更は 0006 で一括対応する。本 issue では変更しない。
