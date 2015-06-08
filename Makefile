.PHONY: clean xref_warnings deps test test_eunit test_inttest

REBAR=$(PWD)/rebar
RETEST=$(PWD)/deps/retest/retest
LOG_LEVEL?=debug
RT_TARGETS?=inttest

all:
	./bootstrap

clean:
	@rm -rf rebar .rebar/erlcinfo ebin/*.beam inttest/rt.work rt.work .eunit

distclean: clean
	@rm -rf deps

debug:
	@./bootstrap debug

check: debug xref dialyzer deps test

xref:
	@./rebar xref

build_plt:
	@./rebar build-plt

dialyzer:
	@./rebar dialyze

binary: VSN = $(shell ./rebar -V)
binary: clean all
	@cp rebar ../rebar.wiki/rebar
	(cd ../rebar.wiki && git commit -m "Update $(VSN)" rebar)

deps:
	@REBAR_EXTRA_DEPS=1 $(REBAR) get-deps
	@(cd deps/retest && $(REBAR) compile escriptize)

test: test_eunit test_inttest

test_eunit: all
	@$(REBAR) eunit

test_inttest: all deps
	@$(RETEST) -l $(LOG_LEVEL) $(RT_TARGETS)

travis: clean debug xref clean all deps test
