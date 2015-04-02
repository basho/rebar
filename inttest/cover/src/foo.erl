-module(foo).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

covered_function() ->
    "I am tested".

uncovered_function() ->
    "I am not tested".

-ifdef(EUNIT).

covered_function_test() ->
    ?assertEqual("I am tested", covered_function()).

-endif.
