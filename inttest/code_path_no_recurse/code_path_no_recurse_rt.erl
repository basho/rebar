%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Tuncer Ayaz
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
-module(code_path_no_recurse_rt).
-export([files/0,
         run/1]).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "rebar.config", "rebar.config"},
     {copy, "src", "src"},
     {copy, "test", "test"},
     {copy, "deps", "deps"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    retest:log(info, "Compile project~n"),
    {ok, _} = retest:sh("./rebar -v compile"),
    retest:log(info, "Run eunit with referenced deps on the code path~n"),
    {ok, _} = retest:sh("./rebar -v eunit"),
    ok.
