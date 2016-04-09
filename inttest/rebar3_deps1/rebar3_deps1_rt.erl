%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar3_deps1_rt).

-compile(export_all).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

%% Test deps with rebar3-type dependencies (that is, dependencies without Regexes)
%% Example: {git, {appname, "git://something/something", {branch, master}}}
files() ->
    [
     %% A application
     {create, "ebin/a.app", app(a, [a])},
     {copy, "a.rebar.config", "rebar.config"},
     {copy, "a.erl", "src/a.erl"},

     %% B application
     {create, "repo/b/ebin/b.app", app(b, [])},
     {copy, "b.hrl", "repo/b/include/b.hrl"},

     {copy, "c.txt", "repo/c/c.txt"}

    ] ++ inttest_utils:rebar_setup().

apply_cmds([], _Params) ->
    ok;
apply_cmds([Cmd | Rest], Params) ->
    io:format("Running: ~s (~p)\n", [Cmd, Params]),
    {ok, _} = retest_sh:run(Cmd, Params),
    apply_cmds(Rest, Params).

run(_Dir) ->
    %% Initialize the dep app as git repos so that dependencies pull
    %% properly
    GitCmds = ["git init",
               "git add -A",
               "git config user.email 'tdeps@example.com'",
               "git config user.name 'tdeps'",
               "git commit -a -m \"Initial Commit\""],
    apply_cmds(GitCmds, [{dir, "repo/b"}]),
    apply_cmds(GitCmds, [{dir, "repo/c"}]),

    {ok, _} = retest_sh:run("./rebar get-deps", []),
    {ok, _} = retest_sh:run("./rebar compile", []),

    true = filelib:is_regular("ebin/a.beam"),
    true = filelib:is_regular("deps/c/c.txt"),
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
