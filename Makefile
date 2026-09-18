.PHONY: all upgrade compile dialyzer test proper efmt-check elint-check clean ci publish

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
