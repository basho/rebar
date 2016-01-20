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
-module(require_vsn_rt).
-export([files/0,
         run/1]).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "rebar.config", "rebar.config"},
     {create, "ebin/require_vsn.app", app(require_vsn, [])}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    SharedExpected = "==> require_vsn_rt \\(compile\\)",
    %% Provoke ABORT due to failed require vsn check.
    retest:log(info, "Check require vsn failure~n"),
    ok = check_output("./rebar compile", should_fail,
                      [SharedExpected, "ERROR: "],
                      ["WARN: "]),
    %% Treat version constraints as warnings.
    retest:log(info, "Check require vsn success with -k/--keep-going~n"),
    ok = check_output("./rebar -k compile", should_succeed,
                      [SharedExpected, "ERROR: "],
                      ["WARN: "]),
    ok.

check_output(Cmd, FailureMode, Expected, Unexpected) ->
    case {retest:sh(Cmd), FailureMode} of
        {{error, _}=Error, should_succeed} ->
            retest:log(error, "cmd '~s' failed:~n~p~n", [Cmd, Error]),
            Error;
        {{ok, Captured}, should_succeed} ->
            Joined = string:join(Captured, "\n"),
            check_output1(Cmd, Joined, Expected, Unexpected);
        {{error, {stopped, {_Rc, Captured}}}, should_fail} ->
            Joined = string:join(Captured, "\n"),
            check_output1(Cmd, Joined, Expected, Unexpected)
    end.

check_output1(Cmd, Captured, Expected, Unexpected) ->
    ReOpts = [{capture, all, list}],
    ExMatches =
        lists:zf(
          fun(Pattern) ->
                  case re:run(Captured, Pattern, ReOpts) of
                      nomatch ->
                          retest:log(error,
                                     "Expected pattern '~s' missing "
                                     "in the following output:~n"
                                     "=== BEGIN ===~n~s~n=== END ===~n",
                                     [Pattern, Captured]),
                          {true, Pattern};
                      {match, _} ->
                          false
                  end
          end, Expected),

    UnExMatches =
        lists:zf(
          fun(Pattern) ->
                  case re:run(Captured, Pattern, ReOpts) of
                      nomatch ->
                          false;
                      {match, [Match]} ->
                          retest:log(
                            console,
                            "Unexpected output when running cmd '~s':~n~s~n",
                            [Cmd, Match]),
                          {true, Match}
                  end
          end, Unexpected),

    case {ExMatches, UnExMatches} of
        {[], []} ->
            ok;
        _ ->
            error
    end.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
