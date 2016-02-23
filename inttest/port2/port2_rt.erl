%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2016 Luis Rascao
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

-module(port2_rt).
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
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile",
                                        [{env, [{"ERL_CFLAGS", "-DTEST3=test3"}]}])),
    TestSo1 = filelib:last_modified("priv/test" ++
                                    shared_library_file_extension(os:type())),
    ?assert(TestSo1 > 0).

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
