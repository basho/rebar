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
-module(erlc_rt).
-export([files/0,
         run/1]).

-include_lib("eunit/include/eunit.hrl").

-define(MODULES,
        [after_first_erl,
         first_xrl,
         first_yrl,
         first_erl,
         foo,
         foo_app,
         foo_sup,
         foo_test_worker,
         foo_worker,
         'SIMPLE-ASN']).

-define(BEAM_FILES,
        ["after_first_erl.beam",
         "first_xrl.beam",
         "first_yrl.beam",
         "first_erl.beam",
         "foo.beam",
         "foo_app.beam",
         "foo_sup.beam",
         "foo_test_worker.beam",
         "foo_worker.beam",
         "SIMPLE-ASN.beam"]).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "rebar.config", "rebar.config"},
     {copy, "rebar-no_debug_info.config", "rebar-no_debug_info.config"},
     {copy, "include", "include"},
     {copy, "extra-include", "extra-include"},
     {copy, "src", "src"},
     {copy, "extra-src", "extra-src"},
     {copy, "mibs", "mibs"},
     {copy, "asn1", "asn1"},
     {create, "ebin/foo.app", app(foo, ?MODULES)},
     {create, "src/._do_not_compile.erl",
              "syntax error\n"
              "this file is here to verify that rebar does not try to compile\n"
              "files like OS X resource forks and should not be processed at all\n"},
     %% deps
     {create, "deps/foobar/ebin/foobar.app", app(foobar, [foobar])},
     {copy, "foobar.erl", "deps/foobar/src/foobar.erl"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    ok = check_beams(true),
    ok = check_debug_info(true),
    MibResult = filename:join(["priv", "mibs", "SIMPLE-MIB.bin"]),
    ?assertMatch(true, filelib:is_regular(MibResult)),
    ?assertMatch({ok, _}, retest_sh:run("./rebar clean", [])),
    ok = check_beams(false),
    ?assertMatch(false, filelib:is_regular(MibResult)),
    ?assertMatch(
       {ok, _},
       retest_sh:run("./rebar -C rebar-no_debug_info.config compile", [])),
    ok = check_beams(true),
    ok = check_debug_info(false),
    ?assertMatch(true, filelib:is_regular(MibResult)),
    %% Regression test for https://github.com/rebar/rebar/issues/249
    %%
    %% Root cause: We didn't have per-project .rebar/erlcinfo but just one in
    %% <base_dir>/.rebar/erlcinfo.
    %%
    %% Solution: Ensure every project has its own .rebar/erlcinfo
    %%
    %% For the bug to happen, the following conditions must be met:
    %%
    %% 1. <base_dir>/rebar.config has erl_first_files
    %% 2. one of the 'first' files depends on another file (in this
    %%    case via -include_lib())
    %% 3. a sub project's rebar.config, if any, has no erl_first_files entry
    %%
    %% Now because erl_first_files is retrieved via rebar_config:get_list/3,
    %% base_dir/rebar.config's erl_first_files is inherited, and because we had
    %% a shared <base_dir>/.rebar/erlcinfo instead of one per project, the
    %% cached entry was reused. Next, while compiling the sub project
    %% rebar_erlc_compiler:needs_compile/3 gets a last modification time of
    %% zero for the 'first' file which does not exist inside the sub project.
    %% This, and the fact that it has at least one dependency, makes
    %% needs_compile/3 return 'true'. The root cause is that we didn't have per
    %% project .rebar/erlcinfo. For <base_dir>/.rebar/erlcinfo to be populated,
    %% base_dir has to be compiled at least once. Therefore, after the first
    %% compile any compile processing the sub project will fail because
    %% needs_compile/3 will always return true for the non-existent 'first'
    %% file.
    ?assertMatch({ok, _}, retest_sh:run("./rebar clean", [])),
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    ok = check_beams(true),
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    ok = check_beams(true),
    ok.

check_beams(Exist) ->
    check_files(Exist, fun filelib:is_regular/1).

check_debug_info(HasDebugInfo) ->
    check_files(HasDebugInfo, fun has_debug_info/1).

check_files(Expected, Check) ->
    lists:foreach(
      fun(F) ->
              File = filename:join("ebin", F),
              ?assertEqual(Expected, Check(File))
      end,
      ?BEAM_FILES).

%% NOTE: Copied from dialyzer_utils:get_abstract_code_from_beam/1 and
%% modified for local use. We could have called the function directly,
%% but dialyzer_utils is not an official API to rely on.
has_debug_info(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_Mod, List}} ->
            case lists:keyfind(abstract_code, 1, List) of
                {abstract_code, {raw_abstract_v1, _Abstr}} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
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
