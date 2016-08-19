%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Tomas Janousek
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

-module(port_rt).
-export([files/0,
         run/1]).

-include_lib("eunit/include/eunit.hrl").

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "rebar.config", "rebar.config"},
     {copy, "c_src", "c_src"},
     {create, "ebin/foo.app", app(foo, [])}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    %% wait a bit for new files to have different timestamps
    wait(),
    %% test.so is created during first compile
    ?assertEqual(0, filelib:last_modified("priv/test.so")),
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    ?assertMatch(true, filelib:is_regular("compile_commands.json")),
    TestSo1 = filelib:last_modified("priv/test" ++
                                    shared_library_file_extension(os:type())),
    ?assert(TestSo1 > 0),
    wait(),
    %% nothing happens during second compile
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    TestSo2 = filelib:last_modified("priv/test" ++
                                    shared_library_file_extension(os:type())),
    Test1o2 = filelib:last_modified("c_src/test1" ++
                                    object_file_extension(os:type())),
    Test2o2 = filelib:last_modified("c_src/test2" ++
                                    object_file_extension(os:type())),
    ?assertEqual(TestSo1, TestSo2),
    ?assert(TestSo1 >= Test1o2),
    ?assert(TestSo1 >= Test2o2),
    wait(),
    %% when test2.c changes, at least test2.o and test.so are rebuilt
    ?assertMatch({ok, _}, retest:run({touch, "c_src/test2.c"}, [{dir, "."}])),
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    TestSo3 = filelib:last_modified("priv/test" ++
                                    shared_library_file_extension(os:type())),
    Test2o3 = filelib:last_modified("c_src/test2" ++
                                    object_file_extension(os:type())),
    ?assert(TestSo3 > TestSo2),
    ?assert(Test2o3 > TestSo2),
    %% detecting the a full recompile is needed when changing a .h file is a feature attained
    %% by using the -MMD gcc flag which sadly is not available in Windows, so this part of the
    %% test is only executed in Unix
    case os:type() of
        {win32, _} -> ok;
        _ ->
            wait(),
            %% when test2.h changes, at least test2.o and test.so are rebuilt
            ?assertMatch({ok, _},
                retest:run({touch, "c_src/test2.h"}, [{dir, "."}])),
            ?assertMatch({ok, _},
                retest_sh:run("./rebar compile", [])),
            TestSo4 = filelib:last_modified("priv/test" ++
                                            shared_library_file_extension(os:type())),
            Test2o4 = filelib:last_modified("c_src/test2" ++
                                            object_file_extension(os:type())),
            ?assert(TestSo4 > TestSo3),
            ?assert(Test2o4 > TestSo3),
            wait(),
            %% when test1.h changes, everything is rebuilt
            ?assertMatch({ok, _},
                retest:run({touch, "c_src/test1.h"}, [{dir, "."}])),
            ?assertMatch({ok, _},
                retest_sh:run("./rebar compile", [])),
            TestSo5 = filelib:last_modified("priv/test" ++
                                            shared_library_file_extension(os:type())),
            Test1o5 = filelib:last_modified("c_src/test1" ++
                                            object_file_extension(os:type())),
            Test2o5 = filelib:last_modified("c_src/test2" ++
                                            object_file_extension(os:type())),
            ?assert(TestSo5 > TestSo4),
            ?assert(Test1o5 > TestSo4),
            ?assert(Test2o5 > TestSo4),
            ok
    end.

wait() ->
    timer:sleep(1000).

object_file_extension({win32, nt}) -> ".o";
object_file_extension(_) -> ".o".

shared_library_file_extension({win32, nt}) -> ".dll";
shared_library_file_extension(_) -> ".so".

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
