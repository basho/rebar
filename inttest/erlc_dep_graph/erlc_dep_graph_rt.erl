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

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "rebar.config", "rebar.config"},
     {copy, "src", "src"},
     {copy, "include", "include"},
     {copy, "extra_include", "extra_include"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    retest_log:log(debug, "compiling all...\n\n", []),
    compile_all(ok, ""),
    retest_log:log(debug, "checking beams integrity...\n\n", []),
    check_beams_ok(),
    check_beams_untouched(filelib:wildcard("ebin/*.beam")),
    retest_log:log(debug, "modifying lisp.erl and recompiling...\n\n", []),
    modify_and_recompile_ok("src/lisp.erl", "ebin/lisp.beam"),

    retest_log:log(debug, "cleaning all...\n\n", []),
    clean_all_ok(),
    retest_log:log(debug, "compiling all (expect fail)...\n\n", []),
    compile_all(error, "-C rebar.config.non-existing"),
    retest_log:log(debug, "compiling all...\n\n", []),
    compile_all(ok, ""),
    retest_log:log(debug, "modifying extra_include/extra.hrl and recompiling...\n\n", []),
    modify_and_recompile_ok("extra_include/extra.hrl", "ebin/java.beam"),

    retest_log:log(debug, "rewriting src/java.erl...\n\n", []),
    Java = "src/java.erl",
    {ok, OrigContent} = file:read_file(Java),
    %% Remove header file inclusion
    {ok, _} = file:copy("src/java.erl.no_extra", Java),
    %% Ensure recompilation
    touch([Java]),
    retest_log:log(debug, "compiling all...\n\n", []),
    compile_all(ok, ""),
    %% Modify that header file
    retest_log:log(debug, "again modifying extra_include/extra.hrl and recompiling...\n\n", []),
    touch(["extra_include/extra.hrl"]),
    %% Ensure we don't have to recompile anything
    retest_log:log(debug, "ensure ebin/java.beam was untouched...\n\n", []),
    check_beams_untouched(["ebin/java.beam"]),
    %% Clean up
    retest_log:log(debug, "modifying src/java.erl...\n\n", []),
    ok = file:write_file(Java, OrigContent),

    %% Check that changes propagate deeply through the dependency tree
    retest_log:log(debug, "modifying include/lambda.hrl...\n\n", []),
    modify_and_recompile_ok("include/lambda.hrl", "ebin/perl.beam"),

    ok.

check_beams_ok() ->
    F = fun(BeamFile) -> ?assert(filelib:is_regular(BeamFile)) end,
    with_erl_beams(F).

check_beams_untouched(Beams) ->
    compile_all_and_assert_mtimes(Beams, fun erlang:'=:='/2).

modify_and_recompile_ok(TouchFile, CheckFile) ->
    touch([TouchFile]),
    compile_all_and_assert_mtimes([CheckFile], fun erlang:'<'/2).

compile_all_and_assert_mtimes(Beams, Cmp) ->
    BeamsModifiedBefore = mtime_ns(Beams),
    compile_all(ok, ""),
    BeamsModifiedAfter = mtime_ns(Beams),
    lists:zipwith(fun(Before, After) ->
                    ?assert(Cmp(Before, After))
                  end,
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
    [calendar:datetime_to_gregorian_seconds(filelib:last_modified(File)) || File <- Files].

touch(Files) ->
    %% Sleep one second so that filelib:last_modified/1 is guaranteed to notice
    %% that files have changed.
    ok = timer:sleep(1000),
    [file:change_time(File, calendar:local_time()) || File <- Files].

compile_all(Result, Opts) ->
    ?assertMatch({Result, _},
        retest_sh:run("./rebar " ++ Opts ++ " compile", [])).

clean_all_ok() ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar clean", [])).
