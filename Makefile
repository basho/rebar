.PHONY: clean dialyzer_warnings xref_warnings deps test test_eunit test_inttest

REBAR=$(PWD)/rebar
RETEST=$(PWD)/deps/retest/retest
OTPVSNCMD='io:fwrite("~s",[rebar_utils:otp_release()]), halt().'
OTPVSN=$(shell erl -pa ebin/ -noshell -eval $(OTPVSNCMD))
PLT_FILENAME=~/.dialyzer_rebar_$(OTPVSN)_plt
LOG_LEVEL?=debug
RT_TARGETS?=inttest

all:
	./bootstrap

clean:
	@rm -rf rebar ebin/*.beam inttest/rt.work rt.work .eunit
	@rm -f .rebarinfo

distclean: clean
	@rm -f dialyzer_warnings
	@rm -rf deps

debug:
	@./bootstrap debug

check: debug xref dialyzer deps test

xref:
	@./rebar xref

build_plt:
	-dialyzer --build_plt --output_plt $(PLT_FILENAME) --apps \
		erts \
		kernel \
		stdlib \
		crypto \
		compiler \
		asn1 \
		eunit \
		tools \
		ssl \
		edoc \
		reltool \
		snmp \
		sasl
	-dialyzer --add_to_plt --plt $(PLT_FILENAME) \
		--output_plt $(PLT_FILENAME) \
		--apps diameter

dialyzer: dialyzer_warnings
	@diff -U0 dialyzer_reference dialyzer_warnings

dialyzer_warnings:
	-@dialyzer --plt $(PLT_FILENAME) -q -nn -n ebin \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		> dialyzer_warnings

typer:
	typer -r --plt $(PLT_FILENAME) ./src -I ./include

binary: VSN = $(shell ./rebar -V)
binary: clean all
	@cp rebar ../rebar.wiki/rebar
	(cd ../rebar.wiki && git commit -m "Update $(VSN)" rebar)

deps:
	@REBAR_EXTRA_DEPS=1 ./rebar get-deps
	$(MAKE) -C deps/retest

test: test_eunit test_inttest

test_eunit: all
	@$(REBAR) eunit

test_inttest: all deps
	@$(RETEST) -l $(LOG_LEVEL) $(RT_TARGETS)

travis: clean debug xref clean all deps test
