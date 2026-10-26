.PHONY: all upgrade compile dialyzer test proper efmt-check elint-check clean ci publish fuzzing

all: clean upgrade compile dialyzer test proper

upgrade:
	@./rebar3 do update, upgrade --all

compile:
	@./rebar3 xref

dialyzer:
	@./rebar3 dialyzer

test:
	@./rebar3 as test eunit, cover

proper:
	@./rebar3 as test proper

# prek.toml 経由で efmt / elint を実行する
efmt-check:
	@prek run efmt-check --all-files

elint-check:
	@prek run elint --all-files

clean:
	@./rebar3 clean

ci: compile dialyzer test proper

publish:
	@./rebar3 hex publish package

# Base32 デコーダーを libFuzzer で fuzzing する。
# fuzzer は OTP 29.1 以上と Clang / compiler-rt を必要とするため CI では実行しない。
FUZZ_CORPUS := _build/fuzz/base32_fuzz_target/corpus
# fuzzing を実行する秒数。0 はクラッシュ検出か中断まで継続する
FUZZ_TIME ?= 30

# 実行設定は rebar.config の fuzzer セクションに置く。
# 初期コーパスだけ実行用ディレクトリーへコピーする。
fuzzing:
	@mkdir -p $(FUZZ_CORPUS)
	@cp -f fuzz/corpus/* $(FUZZ_CORPUS)/
	@./rebar3 as fuzzing fuzzer --max-total-time $(FUZZ_TIME)
