.PHONY: all compile test clean

all: compile

compile:
	rebar3 compile

test:
	rebar3 eunit

clean:
	rebar3 clean

ct:
	rebar3 ct

dialyzer:
	rebar3 dialyzer

mongooseim-start:
	docker run --rm -d -t -h mongooseim-escalus-test-1 --name mongooseim-escalus-test-1 \
		-p 5222:5222 -p 8888:8888  -v `pwd`/mongooseim-escalus-test-1:/member mongooseim/mongooseim:2.1.0beta2

mongooseim-stop:
	docker stop mongooseim-escalus-test-1
