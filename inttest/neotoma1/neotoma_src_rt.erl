%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2015 Luis Rascao (luis.rascao@gmail.com)
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
-module(neotoma_src_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("deps/retest/include/retest.hrl").

-define(GENERATED_MODULES,
        [csv_peg]).

files() ->
    [{copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "mock", "deps"},
     {copy, "src", "src"}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    ?assertMatch({ok, _}, retest:sh("./rebar compile",
                                    [{async, false}])),

    ok = check_files_generated(),

    retest_log:log(debug, "Verify cleanup~n", []),
    ?assertMatch({ok, _}, retest_sh:run("./rebar clean",
                                        [])),
    ok = check_files_deleted(),
    ok.

check_files_generated() ->
    check(fun filelib:is_regular/1,
          generated_erl_files()).

check_files_deleted() ->
    check(fun file_does_not_exist/1,
          generated_erl_files()).

generated_erl_files() ->
    add_dir("src", add_ext(?GENERATED_MODULES, ".erl")).

add_ext(Modules, Ext) ->
    [lists:concat([Module, Ext]) || Module <- Modules].

add_dir(Dir, Files) ->
    [filename:join(Dir, File) || File <- Files].

file_does_not_exist(F) ->
    not filelib:is_regular(F).

check(Check, Files) ->
    lists:foreach(
      fun(F) ->
              ?assertMatch({true, _}, {Check(F), F})
      end,
      Files).
