%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(tdeps_prefer_rt).

-compile(export_all).

setup([Target]) ->
    retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
    ok.

%% Test REBAR_DEPS_FORCE_LIB
%% A -> [B, C]
%% where B should be found globally and C should be found locally
files() ->
    [
    %% A application
    {create, "apps/a/ebin/a.app", app(a, [a])},
    {copy, "a.rebar.config", "apps/a/rebar.config"},
    {template, "a.erl", "apps/a/src/a.erl", dict:from_list([{module, a}])},

    %% B application
    {create, "libs/b-1/ebin/b.app", app(b, [])},
    {copy, "b.hrl", "libs/b-1/include/b.hrl"},

    %% C application
    {create, "repo/c/ebin/c.app", app(c, [])},
    {copy, "c.hrl", "repo/c/include/c.hrl"},

    {copy, "root.rebar.config", "rebar.config"}
    ] ++ inttest_utils:rebar_setup().

apply_cmds([], _Params) ->
    ok;
apply_cmds([Cmd | Rest], Params) ->
    io:format("Running: ~s (~p)\n", [Cmd, Params]),
    {ok, _} = retest_sh:run(Cmd, Params),
    apply_cmds(Rest, Params).

run(Dir) ->
    %% Initialize the c apps as git repos so that dependencies pull
    %% properly
    GitCmds = ["git init",
               "git add -A",
               "git config user.email 'tdeps@example.com'",
               "git config user.name 'tdeps'",
               "git commit -a -m \"Initial Commit\""],
    ErlLibs = filename:join(Dir, "libs"),
    ok = apply_cmds(GitCmds, [{dir, "repo/c"}]),
    Env = [
        {"REBAR_DEPS_PREFER_LIBS", "1"},
        {"ERL_LIBS", ErlLibs}
    ],

    {ok, _} = retest_sh:run("./rebar -v get-deps", [{env, Env}]),
    {ok, _} = retest_sh:run("./rebar -v compile", [{env, Env}]),
    ok.

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
