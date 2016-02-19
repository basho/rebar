-module(test2_SUITE).

-compile(export_all).

all() ->
    [simple_test].

simple_test(Config) ->
    io:format("Test: ~p\n", [Config]).
