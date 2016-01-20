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
-module(profile_rt).
-export([files/0,
         run/1]).

-include_lib("eunit/include/eunit.hrl").

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    inttest_utils:rebar_setup().

run(_Dir) ->
    Cmd = "./rebar list-deps",
    FprofFiles = fprof_files(),
    EflameFiles = ["eflame.trace", "eflame.svg"],

    %% run a simple command (list-deps) without profiling
    SharedExpected = "==> profile_rt \\(list-deps\\)",
    %% run Cmd without profiling
    retest:log(info, "Check '~s' without profiling~n", [Cmd]),
    ok = check(Cmd, should_succeed, [SharedExpected], ["Profiling!"],
               [], FprofFiles ++ EflameFiles),

    %% run Cmd with fprof profiling
    retest:log(info, "Check '~s' with fprof profiling~n", [Cmd]),
    ok = check(Cmd ++ " -p", should_succeed,
               [SharedExpected, "Profiling!", "See fprof\.analysis"],
               ["See eflame\.svg"],
               FprofFiles, EflameFiles),
    delete_files(FprofFiles),

    %% run Cmd with explicitly selected fprof profiling
    retest:log(info, "Check '~s' with explicitly selected fprof profiling~n",
               [Cmd]),
    ok = check(Cmd ++ " -p profiler=fprof", should_succeed,
               [SharedExpected, "Profiling!", "See fprof\.analysis"],
               ["See eflame\.svg"],
               FprofFiles, EflameFiles),
    delete_files(FprofFiles),

    case code:lib_dir(eflame) of
        {error, bad_name} ->
            retest:log(info,
                       "eflame not found in code path. skip eflame test~n"),
            ok;
        _EflameDir ->
            %% run Cmd with eflame profiling
            retest:log(info, "Check '~s' with eflame profiling~n", [Cmd]),
            ok = check(Cmd ++ " -p profiler=eflame", should_succeed,
                       [SharedExpected, "Profiling!", "See eflame\.svg"],
                       ["See fprof\.analysis"],
                       EflameFiles, FprofFiles),
            delete_files(EflameFiles)
    end,

    ok.

fprof_files() ->
    FprofFiles = ["fprof.trace", "fprof.analysis"],
    CgrindFiles = ["fprof.cgrind"],
    case os:find_executable("erlgrind") of
        false ->
            retest:log(info,
                       "erlgrind escript not found. skip fprof.cgrind check~n"),
            FprofFiles;
        _ErlGrind ->
            FprofFiles ++ CgrindFiles
    end.

check(Cmd, FailureMode, ExpectedOutput, UnexpectedOutput,
      ExpectedFiles, UnexpectedFiles) ->
    case {retest:sh(Cmd), FailureMode} of
        {{error, _}=Error, should_succeed} ->
            retest:log(error, "cmd '~s' failed:~n~p~n", [Cmd, Error]),
            Error;
        {{ok, CapturedOutput}, should_succeed} ->
            JoinedOutput = string:join(CapturedOutput, "\n"),
            check1(Cmd, JoinedOutput, ExpectedOutput, UnexpectedOutput,
                   ExpectedFiles, UnexpectedFiles);
        {{error, {stopped, {_Rc, CapturedOutput}}}, should_fail} ->
            JoinedOutput = string:join(CapturedOutput, "\n"),
            check1(Cmd, JoinedOutput, ExpectedOutput, UnexpectedOutput,
                   ExpectedFiles, UnexpectedFiles)
    end.

check1(Cmd, CapturedOutput, ExpectedOutput, UnexpectedOutput,
       ExpectedFiles, UnexpectedFiles) ->
    ReOpts = [{capture, all, list}],
    ExMatches =
        lists:zf(
          fun(Pattern) ->
                  case re:run(CapturedOutput, Pattern, ReOpts) of
                      nomatch ->
                          retest:log(error,
                                     "Expected pattern '~s' missing "
                                     "in the following output:~n"
                                     "=== BEGIN ===~n~s~n=== END ===~n",
                                     [Pattern, CapturedOutput]),
                          {true, Pattern};
                      {match, _} ->
                          false
                  end
          end, ExpectedOutput),

    UnExMatches =
        lists:zf(
          fun(Pattern) ->
                  case re:run(CapturedOutput, Pattern, ReOpts) of
                      nomatch ->
                          false;
                      {match, [Match]} ->
                          retest:log(
                            console,
                            "Unexpected output when running cmd '~s':~n~s~n",
                            [Cmd, Match]),
                          {true, Match}
                  end
          end, UnexpectedOutput),

    ExFiles =
        lists:zf(
          fun(File) ->
                  case filelib:is_regular(File) of
                      true ->
                          false;
                      false ->
                          retest:log(error,
                                     "Expected file missing: ~s~n", [File]),
                          {true, File}
                  end
          end, ExpectedFiles),

    UnExFiles =
        lists:zf(
          fun(File) ->
                  case filelib:is_regular(File) of
                      true ->
                          retest:log(error,
                                     "Unexpected file found: ~s~n", [File]),
                          {true, File};
                      false ->
                          false
                  end
          end, UnexpectedFiles),

    case {ExMatches, UnExMatches, ExFiles, UnExFiles} of
        {[], [], [], []} ->
            ok;
        _ ->
            error
    end.

delete_files(Files) ->
    lists:foreach(fun(File) -> ok = file:delete(File) end, Files).

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
