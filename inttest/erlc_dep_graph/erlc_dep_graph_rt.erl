%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2015 David Kubecka
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(erlc_dep_graph_rt).
-export([files/0,
         run/1]).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "src", "src"},
     {copy, "include", "include"},
     {copy, "extra_include", "extra_include"}].

run(_Dir) ->
    compile_all(ok, ""),
    check_beams_ok(),
    check_beams_untouched(),
    modify_and_recompile_ok("src/lisp.erl", "ebin/lisp.beam"),

    clean_all_ok(),
    compile_all(error, "-C rebar.config.non-existing"),
    compile_all(ok, ""),
    modify_and_recompile_ok("extra_include/extra.hrl", "ebin/java.beam"),

    ok.

check_beams_ok() ->
    F = fun(BeamFile) -> ?assert(filelib:is_regular(BeamFile)) end,
    with_erl_beams(F).

check_beams_untouched() ->
    Beams = filelib:wildcard("ebin/*.beam"),
    compile_all_and_assert_mtimes(Beams, fun erlang:'=:='/2).

modify_and_recompile_ok(TouchFile, CheckFile) ->
    touch([TouchFile]),
    compile_all_and_assert_mtimes([CheckFile], fun erlang:'<'/2).

compile_all_and_assert_mtimes(Beams, Cmp) ->
    BeamsModifiedBefore = mtime_ns(Beams),
    compile_all(ok, ""),
    BeamsModifiedAfter = mtime_ns(Beams),
    lists:zipwith(fun(Before, After) -> ?assert(Cmp(Before, After)) end,
                  BeamsModifiedBefore, BeamsModifiedAfter).

with_erl_beams(F) ->
    lists:map(
        fun(ErlFile) ->
            ErlRoot = filename:rootname(filename:basename(ErlFile)),
            BeamFile = filename:join("ebin", ErlRoot ++ ".beam"),
            F(BeamFile)
        end,
        filelib:wildcard("src/*.erl")).

mtime_ns(Files) ->
    [os:cmd("stat -c%y " ++ File) || File <- Files].

touch(Files) ->
    %% Sleep one second so that filelib:last_modified/1 is guaranteed to notice
    %% that files have changed.
    ok = timer:sleep(1000),
    [os:cmd("touch " ++ File) || File <- Files].

compile_all(Result, Opts) ->
    ?assertMatch({Result, _},
        retest_sh:run("./rebar " ++ Opts ++ " compile", [])).

clean_all_ok() ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar clean", [])).
