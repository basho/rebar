-module(foo_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [simple].

simple(Config) ->
    io:format("Test: ~p\n", [Config]),
    ok.
