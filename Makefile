.PHONY: clean dialyzer_warnings xref_warnings deps test

REBAR=$(PWD)/rebar
RETEST=$(PWD)/deps/retest/retest
OTPVSNCMD='io:fwrite("~s",[rebar_utils:otp_release()]), halt().'
OTPVSN=$(shell erl -pa ebin/ -noshell -eval $(OTPVSNCMD))
PLT_FILENAME=~/.dialyzer_rebar_$(OTPVSN)_plt

all:
	./bootstrap

clean:
	@rm -rf rebar ebin/*.beam inttest/rt.work rt.work .eunit
	@rm -f .rebarinfo

distclean: clean
	@rm -f dialyzer_warnings
	@rm -rf deps
	@rm -f ~/.dialyzer_rebar_*_plt

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
	-dialyzer --add_to_plt --output_plt $(PLT_FILENAME) --apps diameter

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
	@(cd deps/retest && $(REBAR) compile escriptize)

test:
	@$(REBAR) eunit
	@$(RETEST) -v inttest

travis: clean debug xref clean all deps test
