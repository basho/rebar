%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Brian H. Ward
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
-module(cover_rt).

-export([files/0,run/1]).

-include_lib("eunit/include/eunit.hrl").

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {create, "ebin/foo.app", app(foo)},
     {copy, "src", "src"},
     {copy,
      "rebar-cover_export_json.config",
      "rebar-cover_export_json.config"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    ifdef_test(),
    cover_export_json_test(),
    ok.

ifdef_test() ->
    {ok, Output} = retest:sh("./rebar -v eunit"),
    io:format("output => ~p~n", [Output]),
    ?assert(check_output(Output, "foo")),
    {ok, Listing} = file:list_dir(".eunit"),
    ?assert(check_output(Listing, "foo.beam")),
    ?assertMatch({ok,_}, retest:sh("./rebar clean")).

cover_export_json_test() ->
    {ok, Output} =
        retest:sh("./rebar -v -C rebar-cover_export_json.config eunit"),
    ?assert(check_output(Output, "foo")),
    ?assertEqual(
       {ok, <<"{\"module\":\"foo\",\"covered\":2,\"not_covered\":1}">>},
       file:read_file(".eunit/foo.COVER.json")),
    ?assertMatch(
       {ok, _},
       retest:sh("./rebar -C rebar-cover_export_json.config clean")).

check_output(Output,Target) ->
    lists:any(fun(Line) ->
                      string:str(Line, Target) > 0
              end, Output).
app(Name) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, []},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
