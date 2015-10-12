%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Vlad Dumitrescu (vladdu55@gmail.com)
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
%% @author Vlad Dumitrescu <vladdu55@gmail.com>
%% @author Chris Bernard <cebernard@gmail.com>
%% @doc This tests functionality provided by the rebar command 'compile'.
%% -------------------------------------------------------------------
-module(rebar_compiler_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% Assuming this test is run inside the rebar 'eunit'
%% command, the current working directory will be '.eunit'
-define(REBAR_SCRIPT, "../rebar").

-define(TMP_DIR, "tmp_eunit/").

%% ====================================================================
%% Rebar compiler tests
%% ====================================================================

keep_going_test_() ->
    {"Ensure compiler keeps going after finding errors, when --keep-going is requested",
     setup,
     fun() ->
             setup_basic_project(),
             rebar("-k compile")
     end,
     fun teardown/1,
     fun(RebarOut)->
             [
              {"Continue after error",
               ?_assert(string:str(RebarOut, "Continuing after build error") =/= 0)},
              ?_assert(string:str(RebarOut, "ERROR: compile failed") == 0)
             ] ++ assert_files_in("ebin", ["ebin/myapp_mymod2.beam"])
     end
    }.

keep_going_firstfiles_test_() ->
    {"Ensure compiler keeps going after finding errors in erl_first_files, when --keep-going is requested",
     setup,
     fun() ->
             setup_basic_project(),
             setup_rebar_config(),
             rebar("-k compile")
     end,
     fun teardown/1,
     fun(RebarOut)->
             [
              {"Continue after error",
               ?_assert(string:str(RebarOut, "Continuing after build error") =/= 0)},
              ?_assert(string:str(RebarOut, "ERROR: compile failed") == 0)
             ] ++ assert_files_in("ebin", ["ebin/myapp_mymod2.beam"])
     end
    }.

not_keep_going_test_() ->
    {"Ensure compiler stops after finding errors",
     setup,
     fun() ->
             setup_basic_project(),
             setup_rebar_config()
     end,
     fun teardown/1,
     fun()->
             RebarOut = rebar("compile"),
             [
              {"Exit after error",
               ?_assert(string:str(RebarOut, "ERROR: compile failed") =/= 0)}
             ] ++ assert_files_not_in("ebin", ["ebin/myapp_mymod2.beam"])
     end
    }.

%% ====================================================================
%% Setup and Teardown
%% ====================================================================

-define(rebar_config,
        ["{erl_first_files, [\"src/myapp_mymod1.erl\"]}."
        ]).

%% this file has a syntax error
-define(myapp_mymod1,
        ["-module(myapp_mymod1).\n",
         "-export([myfunc/0]).\n",
         "my func() -> [4], ok.\n"]).

-define(myapp_mymod2,
        ["-module(myapp_mymod2).\n",
         "-export([myfunc/0]).\n",
         "myfunc() -> [4], ok.\n"]).


make_tmp_dir() ->
    case file:make_dir(?TMP_DIR) of
        ok ->
            ok;
        {error, eexist} ->
            remove_tmp_dir(),
            make_tmp_dir();
        Error ->
            throw(Error)
    end.

setup_environment() ->
    ok = make_tmp_dir(),
    prepare_rebar_script(),
    ok = file:set_cwd(?TMP_DIR).

setup_rebar_config() ->
    ok = file:write_file("rebar.config", ?rebar_config).

setup_basic_project() ->
    setup_environment(),
    rebar("create-app appid=myapp"),
    ok = file:make_dir("ebin"),
    ok = file:write_file("src/myapp_mymod1.erl", ?myapp_mymod1),
    ok = file:write_file("src/myapp_mymod2.erl", ?myapp_mymod2).

teardown(_) ->
    ok = file:set_cwd(".."),
    ok = remove_tmp_dir().

remove_tmp_dir() ->
    remove_tmp_dir(arg_for_eunit).

remove_tmp_dir(_) ->
    ok = rebar_file_utils:rm_rf(?TMP_DIR).

%% ====================================================================
%% Helper Functions
%% ====================================================================

prepare_rebar_script() ->
    Rebar = ?TMP_DIR ++ "rebar",
    {ok, _} = file:copy(?REBAR_SCRIPT, Rebar),
    case os:type() of
        {unix, _} ->
            [] = os:cmd("chmod u+x " ++ Rebar);
        {win32, _} ->
            {ok, _} = file:copy(?REBAR_SCRIPT ++ ".cmd",
                                ?TMP_DIR ++ "rebar.cmd")
    end.

rebar() ->
    rebar([]).

rebar(Args) when is_list(Args) ->
    Out = os:cmd(filename:nativename("./rebar") ++ " " ++ Args),
    %% ?debugMsg("**** Begin"), ?debugMsg(Out), ?debugMsg("**** End"),
    Out.

assert_dirs_in(Name, [Dir|T]) ->
    [{Name ++ " has directory: " ++ Dir, ?_assert(filelib:is_dir(Dir))} |
         assert_dirs_in(Name, T)];
assert_dirs_in(_, []) -> [].

assert_files_in(Name, [File|T]) ->
    [{Name ++ " has file: " ++ File, ?_assert(filelib:is_regular(File))} |
         assert_files_in(Name, T)];
assert_files_in(_, []) -> [].

assert_files_not_in(Name, [File|T]) ->
    [{Name ++ " does not have file: " ++ File,
      ?_assertNot(filelib:is_regular(File))} | assert_files_not_in(Name, T)];
assert_files_not_in(_, []) -> [].

