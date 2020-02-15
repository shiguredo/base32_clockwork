.PHONY: doc

all:
	rebar3 compile
	rebar3 doc
	rebar3 xref
	rebar3 eunit

compile:
	rebar3 compile

doc:
	rebar3 doc

xref: compile
	rebar3 xref

clean:
	rebar3 clean

#test: xref
test: compile
	rebar3 eunit

