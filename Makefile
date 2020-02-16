.PHONY: doc

all:
	rebar3 compile
	rebar3 xref
	rebar3 eunit

compile:
	rebar3 compile

xref: compile
	rebar3 xref

clean:
	rebar3 clean

test: compile
	rebar3 eunit

