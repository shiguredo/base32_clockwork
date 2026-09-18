# 変更履歴

## develop

### misc

- [CHANGE] GitHub Actions のコンテナイメージをやめて shiguredo/setup-erlang で Erlang/OTP 29.0.6 / AWS-LC v5.8.0 をセットアップし、`minimum_otp_vsn` を 29.0 にする
  - @voluntas
- [CHANGE] rebar3_efmt / rebar3_lint / elvis を削除し、prek (`prek.toml`) 経由の efmt / elint (`shiguredo/erlang-pre-commit` 2026.4.0) に切り替える
  - CI は `j178/prek-action` で `prek.toml` のフックを実行する
  - elint の newline_after_arrow に合わせて symbol/1 と case 節を整形する
  - @voluntas
- [CHANGE] Slack 通知を shiguredo/github-actions の composite action (slack-notify) に移行する
  - @voluntas
- [ADD] PBT を追加
  - clockwork のみ
  - @voluntas
- [UPDATE] rebar3 3.25.1 に更新する
  - @voluntas
- [FIX] import を利用しないようにする
  - @voluntas

## 2023.1.0

日付: 2023-05-02

- [ADD] hex_efmt と hex_elvis を追加する
  - @voluntas
- [CHANGE] efmt を適用する
  - @voluntas
- [CHANGE] rebar3 の minimum_otp_vsn を 26.0 にする
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 26.0-rc3 に上げる
  - @voluntas
- [ADD] slack 通知を secrets.SLACK_INTERNAL_CHANNEL に変更
  - @voluntas
- [ADD] rebar3 3.20.0 を追加する
  - @voluntas
- [ADD] elvis.config を追加する
  - @voluntas

## 2021.3.0

- [CHANGE] applicaiton を base32 に戻す
- [ADD] pkg_name を base32_clockwork にする

## 2021.2.0

- [ADD] rebar3 hex plugin を追加する
- [ADD] rebar3 3.17.0 を追加する
  - @voluntas
- [CHANGE] 最小 OTP を 24.1 に上げる
  - @voluntas

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
