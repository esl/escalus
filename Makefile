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

DEPS_PLT = $(CURDIR)/.deps_plt
DEPS = deps/base16 common_test crypto erts deps/exml kernel deps/lhttpc \
	   ssl stdlib deps/wsecli

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS)

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions \
		     ./deps/exml/ebin ./ebin -o dialyzer.log

typer:
	typer -I deps -I include -I src --plt $(DEPS_PLT) -r ./src
