.PHONY: all test tags bootstrap clean

all: tags compile

test:
	./rebar skip_deps=true ct

tags:
	ctags **/*.erl **/*.hrl

compile:
	./rebar skip_deps=true compile

bootstrap:
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean
