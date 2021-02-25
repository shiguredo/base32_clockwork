# 変更履歴

## develop

## 2021.1

- [FIX] `base32:decode(clockwork, _)` が Crockford になっていた問題を修正する
    - @shino
- [CHANGE] `base32:decode(crockford, _)` が Binary を返していたため `{ok, Binary}` に変更する
    - @shino

## 2020.2

- [CHANGE] Clockwork Base32 を実装する
    - @szktty

## 2020.1.2

- [FIX] Crockford's Base32: エンコード後の文字列の先頭が "0" であるべき場合に "0" が抜け落ちる事象を修正する
    - @szktty

## 2020.1.1

- [UPDATE] Crockford 版を数値計算で再実装
    - @szktty

## 2020.1

**YYYY.RELEASE[.FIX] にバージョン番号表記を変更**

リリースミス

## 1.1.0

- [ADD] crockford の decode を追加
    - @szktty

## 1.0.0

**リリース**
