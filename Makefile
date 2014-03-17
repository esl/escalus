.PHONY: all compile test clean

all: compile

compile: rebar
	./rebar get-deps compile

test: rebar compile
	./rebar skip_deps=true eunit

clean: rebar
	./rebar clean

rebar:
	wget https://github.com/rebar/rebar/releases/download/2.2.0/rebar
	chmod u+x rebar
