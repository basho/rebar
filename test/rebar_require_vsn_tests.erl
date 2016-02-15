%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_require_vsn_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

version_tuple_test_() ->
    [%% typical cases
     ?_assert(check("R14A", "eunit") =:= {14, 0, 0}),
     ?_assert(check("R14B", "eunit") =:= {14, 0, 0}),
     ?_assert(check("R14B01", "eunit") =:= {14, 1, 0}),
     ?_assert(check("R14B02", "eunit") =:= {14, 2, 0}),
     ?_assert(check("R14B03", "eunit") =:= {14, 3, 0}),
     ?_assert(check("R14B04", "eunit") =:= {14, 4, 0}),
     ?_assert(check("R15B", "eunit") =:= {15, 0, 0}),
     ?_assert(check("R15B01", "eunit") =:= {15, 1, 0}),
     ?_assert(check("R15B02", "eunit") =:= {15, 2, 0}),
     ?_assert(check("R15B03-1", "eunit") =:= {15, 3, 1}),
     ?_assert(check("R15B03", "eunit") =:= {15, 3, 0}),
     ?_assert(check("R16B", "eunit") =:= {16, 0, 0}),
     ?_assert(check("R16B01", "eunit") =:= {16, 1, 0}),
     ?_assert(check("R16B02", "eunit") =:= {16, 2, 0}),
     ?_assert(check("R16B03", "eunit") =:= {16, 3, 0}),
     ?_assert(check("R16B03-1", "eunit") =:= {16, 3, 1}),
     ?_assert(check("17", "eunit") =:= {17, 0, 0}),
     ?_assert(check("17.0", "eunit") =:= {17, 0, 0}),
     ?_assert(check("17.1", "eunit") =:= {17, 1, 0}),
     ?_assert(check("17.3", "eunit") =:= {17, 3, 0}),
     ?_assert(check("17.4", "eunit") =:= {17, 4, 0}),
     ?_assert(check("17.5", "eunit") =:= {17, 5, 0}),
     ?_assert(check("18.0", "eunit") =:= {18, 0, 0}),
     ?_assert(check("18.1", "eunit") =:= {18, 1, 0}),
     ?_assert(check("18.2", "eunit") =:= {18, 2, 0}),
     ?_assert(check("18.2.1", "eunit") =:= {18, 2, 1}),
     %% error cases
     ?_assertException(throw, rebar_abort, check("", "eunit")),
     ?_assertException(throw, rebar_abort, check("abc", "eunit"))
    ].

check(OtpRelease, Type) ->
    rebar_require_vsn:version_tuple(abort, OtpRelease, Type).
