%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(ct1_rt).

-compile(export_all).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {create, "ebin/a1.app", app(a1)},
     {copy, "rebar.config", "rebar.config"},
     {copy, "app.config", "app.config"},
     {copy, "test_SUITE.erl", "itest/test_SUITE.erl"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    {ok, _} = retest:sh("./rebar compile ct"),
    {ok, _} = retest:sh("./rebar compile ct -v"),
    ok.

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
