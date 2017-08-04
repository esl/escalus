.PHONY: all compile test clean

all: compile

compile: rebar
	@./rebar get-deps compile > $@.log 2>&1 \
		|| (echo "$@ failed"; cat $@.log; exit 1)

test: rebar compile
	@./rebar skip_deps=true eunit > $@.log 2>&1 \
		&& (printf "%s: " $@; tail -1 $@.log) \
		|| (echo "$@ failed"; cat $@.log; exit 2)

clean: rebar
	@./rebar clean

# Usage: make ct SUITE=user_db_module_SUITE
ct:	compile logs
	@./run_ct SUITE=$(SUITE) > $@.log 2>&1 \
		&& grep "TEST COMPLETE" $@.log \
		|| (cat $@.log; exit 3)

logs:
	@mkdir -p logs

rebar:
	wget https://github.com/rebar/rebar/releases/download/2.2.0/rebar
	chmod u+x rebar

OTP_APPS = kernel stdlib crypto common_test ssl erts public_key
deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps $(OTP_APPS) \
		> erlang_plt.log 2>&1; status=$$? ; \
	if [ $$status -ne 2 ]; then (cat erlang_plt.log; exit $$status); else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps) > deps_plt.log 2>&1; status=$$? ; \
	if [ $$status -ne 2 ]; then (cat deps_plt.log; exit $$status); else exit 0; fi

dialyzer/escalus.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/escalus.plt \
	-o dialyzer/escalus.log ebin > escalus_plt.log; status=$$? ; \
	if [ $$status -ne 2 ]; then (cat escalus_plt.log; exit $$status); else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log \
		> $@.log 2>&1; status=$$? ; \
	if [ $$status -ne 2 ]; then (cat $@.log; exit $$status); else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

escalus_plt: dialyzer/escalus.plt
	@dialyzer --plt dialyzer/escalus.plt --check_plt -o dialyzer/escalus.log \
		> $@.log 2>&1; status=$$? ; \
	if [ $$status -ne 2 ]; then (cat $@.log; exit $$status); else exit 0; fi

dialyzer: erlang_plt deps_plt escalus_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings ebin;

mongooseim-start:
	docker run -d -t -h mongooseim-escalus-test-1 --name mongooseim-escalus-test-1 -p 5222:5222 \
		-v `pwd`/mongooseim-escalus-test-1:/member mongooseim/mongooseim:2.1.0beta2


