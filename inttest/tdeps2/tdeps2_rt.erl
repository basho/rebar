-module(tdeps2_rt).

-compile(export_all).

%% Exercise transitive dependencies where there are multiple files
%% depending on the same set of deps
%% [A1, A2] -> B -> C ; A1 and A2 includes B.hrl which includes C.hrl

files() ->
    [
     %% A1 application
     {create, "apps/a1/ebin/a1.app", app(a1, [a1])},
     {copy, "a.rebar.config", "apps/a1/rebar.config"},
     {template, "a.erl", "apps/a1/src/a1.erl", dict:from_list([{module, a1}])},

     %% A2 application
     {create, "apps/a2/ebin/a2.app", app(a2, [a2])},
     {copy, "a.rebar.config", "apps/a2/rebar.config"},
     {template, "a.erl", "apps/a2/src/a2.erl", dict:from_list([{module, a2}])},

     {copy, "root.rebar.config", "rebar.config"},
     {copy, "../../rebar", "rebar"},

     %% B application
     {create, "repo/b/ebin/b.app", app(b, [])},
     {copy, "b.rebar.config", "repo/b/rebar.config"},
     {copy, "b.hrl", "repo/b/include/b.hrl"},

     %% C application
     {create, "repo/c/ebin/c.app", app(c, [])},
     {copy, "c.hrl", "repo/c/include/c.hrl"}
    ].

run(_Dir) ->
    %% Initialize the b/c apps as mercurial repos so that dependencies pull
    %% properly
    HgCmd = "/bin/sh -c \"hg init && hg add && hg commit -m 'Initial commit'\"",
    {ok, _} = retest_sh:run(HgCmd, [{dir, "repo/b"}]),
    {ok, _} = retest_sh:run(HgCmd, [{dir, "repo/c"}]),

    {ok, _} = retest_sh:run("./rebar -v get-deps compile", []),
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
