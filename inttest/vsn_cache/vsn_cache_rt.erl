%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(vsn_cache_rt).

-compile(export_all).

setup([Target]) ->
    retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
    ok.

files() ->
    [
    %% Cache save check
    {create, "save/src/save.app.src", app(save, [main])},
    {create, "save/vsn_cache_file", ""},
    {copy, "main.erl", "save/src/main.erl"},

    %% Cache load check
    {create, "load/src/load.app.src", app(load, [main])},
    {copy, "main.erl", "load/src/main.erl"}
    ] ++ inttest_utils:rebar_setup().

apply_cmds([], _Params) ->
    ok;
apply_cmds([Cmd | Rest], Params) ->
    io:format("Running: ~s (~p)\n", [Cmd, Params]),
    {ok, _} = retest_sh:run(Cmd, Params),
    apply_cmds(Rest, Params).

run_save(Dir) ->
    GitCmds = ["git init",
               "git add -A",
               "git config user.email 'vsn_cache@example.com'",
               "git config user.name 'vsn_cache'",
               "git commit -a -m \"Initial Commit\""],
    AppDir = filename:join(Dir, "save"),
    EbinDir = filename:join(AppDir, "ebin"),
    AppFile = filename:join(EbinDir, "save.app"),
    VsnCacheFile = filename:join(AppDir, "vsn_cache_file"),
    Env = [{"REBAR_VSN_CACHE_FILE", VsnCacheFile}],

    %% Initialize test git repository 
    ok = apply_cmds(GitCmds, [{dir, AppDir}]),
    %% Compile test project with vsn cache enabled
    {ok, _} = retest_sh:run("../rebar -v compile", [{env, Env}, {dir, AppDir}]),
    %% Vsn cache file has an entry
    {ok, [{{git, AppDir}, Hash}]} = file:consult(VsnCacheFile),
    %% This vsn entry must coincide with entry from ebin/save.app
    {ok, [{application, save, PropList}]} = file:consult(AppFile),
    Hash = proplists:get_value(vsn, PropList),
    ok.

run_load(Dir) ->
    AppDir = filename:join(Dir, "load"),
    EbinDir = filename:join(AppDir, "ebin"),
    AppFile = filename:join(EbinDir, "load.app"),
    VsnCacheFile = filename:join(AppDir, "vsn_cache_file"),
    Hash = "deadbeef",
    CacheEntries = [{{git, AppDir}, Hash}],
    Env = [{"REBAR_VSN_CACHE_FILE", VsnCacheFile}],

    %% Initialize dummy vsn cache file
    vsn_cache_file(VsnCacheFile, CacheEntries),
    %% Compile test project with vsn cache enabled
    {ok, _} = retest_sh:run("../rebar -v compile", [{env, Env}, {dir, AppDir}]),
    %% This vsn entry in cache file must coincide with entry in ebin/load.app
    {ok, [{application, load, PropList}]} = file:consult(AppFile),
    Hash = proplists:get_value(vsn, PropList),
    ok.

run(Dir) ->
    run_save(Dir),
    run_load(Dir),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, git},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

vsn_cache_file(Name, Entries) ->
    file:write_file(Name,
        [io_lib:format("~p.~n", [X]) || X <- Entries]).
