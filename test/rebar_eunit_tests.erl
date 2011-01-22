%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
%% @author Chris Bernard <cebernard@gmail.com>
%% @doc This tests functionality provided by the rebar command 'eunit'.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_eunit_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% Assuming this test is run inside the rebar 'eunit'
%% command, the current working directory will be '.eunit'
-define(REBAR_SCRIPT, "../rebar").

-define(TMP_DIR, "tmp_eunit/").

%% ====================================================================
%% Rebar EUnit and Cover Tests
%% ====================================================================

eunit_test_() ->
    {"Ensure EUnit runs with tests in a 'test' dir and no defined suite",
     setup, fun() -> setup_basic_project(), rebar("-v eunit") end,
     fun teardown/1,
     fun(RebarOut) ->
             [{"Tests in 'test' directory are found and run",
               ?_assert(string:str(RebarOut, "myapp_mymod_tests:") =/= 0)},
              
              {"Tests in 'src' directory are found and run",
               ?_assert(string:str(RebarOut, "myapp_mymod:") =/= 0)},
              
              {"Tests are only run once",
               ?_assert(string:str(RebarOut, "All 2 tests passed") =/= 0)}]
     end}. 

cover_test_() ->
    {"Ensure Cover runs with tests in a test dir and no defined suite",
     setup, fun() -> setup_cover_project(), rebar("-v eunit") end,
     fun teardown/1,

     [{"All cover reports are generated",
       assert_files_in("the temporary eunit directory",
                       expected_cover_generated_files())},
      
      {"Only production modules get coverage reports",
       assert_files_not_in("the temporary eunit directory",
                           [".eunit/myapp_mymod_tests.COVER.html"])}]}.

cover_with_suite_test_() ->
    {"Ensure Cover runs with Tests in a test dir and a test suite",
     setup,
     fun() ->
             setup_cover_project_with_suite(),
             rebar("-v eunit suite=mysuite")
     end,
     fun teardown/1,

     [{"All cover reports are generated",
       assert_files_in("the temporary eunit directory",
                       expected_cover_generated_files())},

      {"Only production modules get coverage reports",
       assert_files_not_in("the temporary eunit directory",
                           [".eunit/myapp_mymod_tests.COVER.html",
                            ".eunit/mysuite.COVER.html"])}]}.

expected_cover_generated_files() ->
    [".eunit/index.html",
     ".eunit/myapp_app.COVER.html",
     ".eunit/myapp_mymod.COVER.html",
     ".eunit/myapp_sup.COVER.html"].

cover_coverage_test_() ->
    {"Coverage is accurately calculated",
     setup, fun() -> setup_cover_project(), rebar("-v eunit") end,
     fun teardown/1,

     [{"Modules that include the EUnit header can still have 100% coverage",
       %% cover notices the implicit EUnit test/0 func that never gets
       %% called during eunit:test(TestRepresentation), so NotCounted
       %% needs to be decremented in this case.
       assert_full_coverage("myapp_mymod")}]}.

deps_outside_startdir_test_() ->
    {"Handling dependencies installed outside the top level build directory",
      setup, fun setup_project_with_external_deps/0,
      fun teardown_project_with_external_deps/1,
      [{"External installs should be ignored during 'clean' command execution",
        fun() ->
            [DepsDir] = get_test_deps_dir(),
            rebar_utils:sh("rebar -v check-deps clean", [return_on_error, 
                {env, [{"ERL_LIBS", filename:rootname(DepsDir, "libfoo")}]},
                {use_stdout, false}]),
            Ebin = filename:join(DepsDir, "ebin"),
            ?assert(filelib:is_file(filename:join(Ebin, "libfoo.beam")))
        end}]}.

%% ====================================================================
%% Environment and Setup Tests
%% ====================================================================

environment_test_() ->
    {"Sanity check the testing environment",
     setup, fun make_tmp_dir/0, fun remove_tmp_dir/1,

     [{"Ensure a test project can be created", 
       ?_assert(filelib:is_dir(?TMP_DIR))},

      {"Ensure the rebar script can be found, copied, and run",
       [?_assert(filelib:is_file(?REBAR_SCRIPT)),
        fun assert_rebar_runs/0]}]}.

assert_rebar_runs() ->
    prepare_rebar_script(),
    ?assert(string:str(os:cmd(filename:nativename("./" ++ ?TMP_DIR ++ "rebar")),
                       "No command to run specified!") =/= 0).

basic_setup_test_() ->
    {"Create a simple project with a 'test' directory, a test, and a module",
     setup, fun setup_basic_project/0, fun teardown/1,

     %% Test the setup function
     assert_dirs_in("Basic Project",
                    ["src", "ebin", "test"]) ++
     assert_files_in("Basic Project",
                     ["test/myapp_mymod_tests.erl", "src/myapp_mymod.erl"])}.

%% ====================================================================
%% Setup and Teardown
%% ====================================================================

-define(myapp_mymod, 
        ["-module(myapp_mymod).\n",
         "-export([myfunc/0]).\n",
         "-include_lib(\"eunit/include/eunit.hrl\").\n",
         "myfunc() -> ok.\n",
         "myprivate_test() -> ?assert(true).\n"]).

-define(myapp_mymod_tests, 
        ["-module(myapp_mymod_tests).\n",
         "-compile([export_all]).\n",
         "-include_lib(\"eunit/include/eunit.hrl\").\n",
         "myfunc_test() -> ?assertMatch(ok, myapp_mymod:myfunc()).\n"]).

-define(mysuite,
        ["-module(mysuite).\n",
         "-export([all_test_/0]).\n",
         "-include_lib(\"eunit/include/eunit.hrl\").\n",
         "all_test_() -> [myapp_mymod_defined_in_mysuite_tests].\n"]).

-define(myapp_mymod_defined_in_mysuite_tests, 
        ["-module(myapp_mymod_defined_in_mysuite_tests).\n",
         "-compile([export_all]).\n",
         "-include_lib(\"eunit/include/eunit.hrl\").\n",
         "myfunc_test() -> ?assertMatch(ok, myapp_mymod:myfunc()).\n"]).

-define(myrebar_config_with_external_deps_dir,
        "{deps, [libfoo]}.\n{deps_dir, [~p]}.\n").

-define(empty_beam, <<"ignored">>).

app(Name) ->
   App = {application, Name,
          [{description, atom_to_list(Name)},
           {vsn, "1"},
           {modules, []},
           {registered, []},
           {applications, [kernel, stdlib]}]},
   io_lib:format("~p.\n", [App]).

make_tmp_dir() ->
    file:make_dir(?TMP_DIR).

get_test_deps_dir() ->
    {ok, PList} = file:consult("rebar.config"),
    proplists:get_value(deps_dir, PList).

setup_environment() ->
    make_tmp_dir(),
    prepare_rebar_script(),
    file:set_cwd(?TMP_DIR).

setup_basic_project() ->
    setup_environment(),
    rebar("create-app appid=myapp"),
    file:make_dir("ebin"),
    file:make_dir("test"),
    file:write_file("test/myapp_mymod_tests.erl", ?myapp_mymod_tests),
    file:write_file("src/myapp_mymod.erl", ?myapp_mymod).

setup_cover_project() ->
    setup_basic_project(),
    file:write_file("rebar.config", "{cover_enabled, true}.\n").

setup_cover_project_with_suite() ->
    setup_cover_project(),
    file:write_file("test/mysuite.erl", ?mysuite),
    file:write_file("test/myapp_mymod_defined_in_mysuite_tests.erl",
                    ?myapp_mymod_defined_in_mysuite_tests).

setup_project_with_external_deps() ->
    setup_environment(),
    TmpDir = rebar_utils:get_temp_filename(),
    file:make_dir(TmpDir),
    DepBaseDir = filename:join(TmpDir, "libfoo"),
    file:make_dir(DepBaseDir),
    DepEbin = filename:join(DepBaseDir, "ebin"),
    file:make_dir(DepEbin),
    file:write_file(filename:join(DepEbin, "libfoo.app"), app(libfoo)),
    file:write_file(filename:join(DepEbin, "libfoo.beam"), ?empty_beam),
    RebarConfig = lists:flatten(io_lib:format(
        ?myrebar_config_with_external_deps_dir, [DepBaseDir])),
    file:write_file("rebar.config", RebarConfig).

teardown_project_with_external_deps(_) ->
    [DepsDir] = get_test_deps_dir(),
    rebar_utils:remove_dir(DepsDir),
    teardown(arg_for_eunit).

teardown(_) ->
    file:set_cwd(".."),
    remove_tmp_dir(),
    ok.

remove_tmp_dir() ->
    remove_tmp_dir(arg_for_eunit).

remove_tmp_dir(_) ->    
    case os:type() of
        {unix, _} ->
            os:cmd("rm -rf " ++ ?TMP_DIR ++ " 2>/dev/null");
        {win32, _} ->
            os:cmd("rmdir /S /Q " ++ filename:nativename(?TMP_DIR))
    end.

%% ====================================================================
%% Helper Functions
%% ====================================================================

prepare_rebar_script() ->
    {ok, _} = file:copy(?REBAR_SCRIPT, ?TMP_DIR ++ "rebar"),
    case os:type() of
        {unix, _} ->
            [] = os:cmd("chmod u+x " ++ ?TMP_DIR ++ "rebar");
        {win32, _} ->
            {ok, _} = file:copy(?REBAR_SCRIPT ++ ".bat",
                                ?TMP_DIR ++ "rebar.bat")
    end.

rebar() ->
    rebar([]).

rebar(Args) when is_list(Args) ->
    Out = os:cmd(filename:nativename("./rebar") ++ " " ++ Args),
    %?debugMsg("**** Begin"), ?debugMsg(Out), ?debugMsg("**** End"),
    Out.

assert_dirs_in(Name, [Dir|T]) ->
    [{Name ++ " has directory: " ++ Dir, ?_assert(filelib:is_dir(Dir))} |
     assert_dirs_in(Name, T)];                                                                         
assert_dirs_in(_, []) -> [].

assert_files_in(Name, [File|T]) ->
    [{Name ++ " has file: " ++ File, ?_assert(filelib:is_file(File))} |
     assert_files_in(Name, T)];   
assert_files_in(_, []) -> [].

assert_files_not_in(Name, [File|T]) ->
    [{Name ++ " does not have file: " ++ File, ?_assertNot(filelib:is_file(File))} |
     assert_files_not_in(Name, T)];   
assert_files_not_in(_, []) -> [].

assert_full_coverage(Mod) ->
    fun() ->
            {ok, F} = file:read_file(".eunit/index.html"),
            Result = [X || X <- string:tokens(binary_to_list(F), "\n"),
                           string:str(X, Mod) =/= 0,
                           string:str(X, "100%") =/= 0],
            file:close(F),
            ?assert(length(Result) =:= 1)
    end.
