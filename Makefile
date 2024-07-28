.PHONY: all upgrade compile dialyzer test proper clean ci publish

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

clean:
	@./rebar3 clean

ci: compile dialyzer test proper

publish:
	@./rebar3 hex publish package
