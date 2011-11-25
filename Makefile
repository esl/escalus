.PHONY: all compile test clean

all: compile

compile:
	./rebar get-deps compile

test: compile
	./rebar skip_deps=true eunit

clean:
	./rebar clean
