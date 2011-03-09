.PHONY: all test compile tags clean

SOURCES=$(wildcard src/*.erl)
INCLUDES=$(wildcard include/*.hrl)
BEAMS=$(patsubst src/%,ebin/%,$(SOURCES:.erl=.beam))
EXMPP_BUILT=deps/exmpp/ebin/exmpp.app

all: tags compile

tags:
	ctags **/*.erl **/*.hrl 2>/dev/null||:

compile: $(EXMPP_BUILT) $(BEAMS) ebin/escalus.app

ebin/%.beam: src/%.erl $(INCLUDES)
	@test -d ebin || mkdir ebin
	erlc -I include -pa deps/exmpp -o ebin $<

ebin/escalus.app: src/escalus.app.src
	@test -d ebin || mkdir ebin
	cp $< $@

clean:
	rm -f ebin/*

## exmpp compilation for fun & profit

$(EXMPP_BUILT): deps/exmpp/Makefile
	cd deps/exmpp; make

deps/exmpp/Makefile: deps/exmpp/configure
	cd deps/exmpp; ./configure

deps/exmpp/configure: deps/exmpp/configure.ac
	cd deps/exmpp; autoreconf -vif

deps/exmpp/configure.ac:
	@test -d deps || mkdir deps
	cd $<; git clone https://github.com/processone/exmpp.git
