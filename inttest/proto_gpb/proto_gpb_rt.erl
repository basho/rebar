%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Luis Rascão (luis.rascao@gmail.com)
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
-module(proto_gpb_rt).
-export([files/0,
         run/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("deps/retest/include/retest.hrl").

-define(MODULES,
        [foo,
         foo_app,
         foo_sup]).

-define(GENERATED_MODULES,
        [test_gpb,
         test2_gpb,
         test3_gpb,
         test4_gpb,
         test5_gpb]).

-define(SOURCE_PROTO_FILES,
        ["test.proto",
         "a/test2.proto",
         "a/b/test3.proto",
         "c/test4.proto",
         "c/d/test5.proto"]).

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "rebar2.config", "rebar2.config"},
     {copy, "include", "include"},
     {copy, "src", "src"},
     {copy, "proto", "proto"},
     {copy, "mock", "deps"},
     {create, "ebin/foo.app", app(foo, ?MODULES ++ ?GENERATED_MODULES)}
    ].

run(_Dir) ->
    % perform test obtaining the .proto files from src dir
    ok = run_from_dir("src", "rebar.config"),
    % perform test obtaining the .proto files from proto dir
    ok = run_from_dir("proto", "rebar2.config").

run_from_dir(ProtoDir, ConfigFile) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar --config "
                                        ++ ConfigFile
                                        ++ " clean",
                                        [])),
    ?assertMatch({ok, _}, retest_sh:run("./rebar --config "
                                        ++ ConfigFile
                                        ++ " compile",
                                        [])),
    %% Foo includes test_gpb.hrl,
    %% So if it compiled, that also means gpb succeeded in
    %% generating the test_gpb.hrl file, and also that it generated
    %% the .hrl file was generated before foo was compiled.
    ok = check_beams_generated(),

    ?DEBUG("Verifying recompilation~n", []),
    TestErl = hd(generated_erl_files()),
    TestProto = hd(source_proto_files(ProtoDir)),
    make_proto_newer_than_erl(TestProto, TestErl),
    TestMTime1 = read_mtime(TestErl),
    ?assertMatch({ok, _}, retest_sh:run("./rebar --config "
                                        ++ ConfigFile
                                        ++ " compile",
                                        [])),
    TestMTime2 = read_mtime(TestErl),
    ?assert(TestMTime2 > TestMTime1),

    ?DEBUG("Verifying recompilation with no changes~n", []),
    TestMTime3 = read_mtime(TestErl),
    ?assertMatch({ok, _}, retest_sh:run("./rebar --config "
                                        ++ ConfigFile
                                        ++ " compile",
                                        [])),
    TestMTime4 = read_mtime(TestErl),
    ?assert(TestMTime3 =:= TestMTime4),

    ?DEBUG("Verify cleanup~n", []),
    ?assertMatch({ok, _}, retest_sh:run("./rebar --config "
                                        ++ ConfigFile
                                        ++ " clean",
                                        [])),
    ok = check_files_deleted(),
    ok.

check_beams_generated() ->
    check(fun filelib:is_regular/1,
          beam_files()).

check_files_deleted() ->
    check(fun file_does_not_exist/1,
          beam_files() ++ generated_erl_files() ++ generated_hrl_files()).

beam_files() ->
    add_dir("ebin", add_ext(?MODULES, ".beam")).

generated_erl_files() ->
    add_dir("src", add_ext(?GENERATED_MODULES, ".erl")).

generated_hrl_files() ->
    add_dir("include", add_ext(?GENERATED_MODULES, ".hrl")).

generated_beam_files() ->
    add_dir("ebin", add_ext(?GENERATED_MODULES, ".beam")).

source_proto_files(ProtoDir) ->
    add_dir(ProtoDir, ?SOURCE_PROTO_FILES).

file_does_not_exist(F) ->
    not filelib:is_regular(F).

add_ext(Modules, Ext) ->
    [lists:concat([Module, Ext]) || Module <- Modules].

add_dir(Dir, Files) ->
    [filename:join(Dir, File) || File <- Files].

read_mtime(File) ->
    {ok, #file_info{mtime=MTime}} = file:read_file_info(File),
    MTime.


make_proto_newer_than_erl(Proto, Erl) ->
    %% Do this by back-dating the erl file instead of touching the
    %% proto file.  Do this instead of sleeping for a second to get a
    %% reliable test.  Sleeping would have been needed sin ce the
    %% #file_info{} (used by eg. filelib:last_modified) does not have
    %% sub-second resolution (even though most file systems have).
    {ok, #file_info{mtime=ProtoMTime}} = file:read_file_info(Proto),
    {ok, ErlInfo} = file:read_file_info(Erl),
    OlderMTime = update_seconds_to_datetime(ProtoMTime, -2),
    OlderErlInfo = ErlInfo#file_info{mtime = OlderMTime},
    ok = file:write_file_info(Erl, OlderErlInfo).

update_seconds_to_datetime(DT, ToAdd) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(DT) + ToAdd).

touch_file(File) ->
    ?assertMatch({ok, _}, retest_sh:run("touch " ++ File, [])).

check(Check, Files) ->
    lists:foreach(
      fun(F) ->
              ?assertMatch({true, _}, {Check(F), F})
      end,
      Files).

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib, gpb]}]},
    io_lib:format("~p.\n", [App]).
