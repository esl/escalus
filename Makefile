.PHONY: all compile test clean

REBAR = ./rebar3

all: compile

compile:
	$(REBAR) compile

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean

ct:
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

mongooseim-start:
	docker run --rm -d -t -h mongooseim-escalus-test-1 --name mongooseim-escalus-test-1 \
		-p 5222:5222 -p 8888:8888  -v `pwd`/mongooseim-escalus-test-1:/member mongooseim/mongooseim:2.1.0beta2

mongooseim-stop:
	docker stop mongooseim-escalus-test-1
