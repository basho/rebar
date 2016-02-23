%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(eunit_surefire_rt).
-export([files/0, run/1]).

-include_lib("eunit/include/eunit.hrl").

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {create, "ebin/foo.app", app(foo)},
     {copy, "src", "src"},
     {copy, "eunit_src", "eunit_src"},
     {copy, "rebar.config"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    {ok, Output} = retest:sh("./rebar -v eunit tests=bar"),
    ?assert(check_output(Output, "bar_test")),
    ok.

check_output(Output, Target) ->
    lists:any(fun(Line) ->
                      string:str(Line, Target) > 0
              end, Output).

%%
%% Generate the contents of a simple .app file
%%
app(Name) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, []},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
