-module(t_skipdeps_cmds_rt).

-compile(export_all).

%% Copy of tdeps1, checks that skippable commands do not run in deps dirs

files() ->
    [
     %% Dummy global config
     {copy, "global.config", ".home_folder/.rebar/config"},
     
     %% A application
     {create, "ebin/a.app", app(a, [a])},
     {create, "empty.config", ""},
     {copy, "a.rebar.config", "rebar.config"},
     {copy, "a.erl", "src/a.erl"},
     {copy, "../../rebar", "rebar"},

     %% B application
     {create, "repo/b/ebin/b.app", app(b, [b])},
     {copy, "b.rebar.config", "repo/b/rebar.config"},
     {copy, "b.erl", "repo/b/src/b.erl"},

     %% C application
     {create, "repo/c/ebin/c.app", app(c, [c])},
     {copy, "c.erl", "repo/c/src/c.erl"}
    ].

run(_Dir) ->
    %% Initialize the b/c apps as mercurial repos so that dependencies pull
    %% properly
    HgCmd = "/bin/sh -c \"hg init && hg add && hg commit -m 'Initial commit'\"",
    {ok, _} = retest_sh:run(HgCmd, [{dir, "repo/b"}]),
    {ok, _} = retest_sh:run(HgCmd, [{dir, "repo/c"}]),

    {ok, _} = retest_sh:run("./rebar get-deps compile", []),

    true = filelib:is_regular("ebin/a.beam"),
    Ref = retest_sh:run("./rebar clean xref -v", [{async, true}]),
    {ok, _} = retest:sh_expect(Ref, ".*Warning: function hello/0"
                                    " is unused export.*", [{newline, any}]),
    false = filelib:is_regular("ebin/a.beam"),
    true = filelib:is_regular("deps/b/ebin/b.beam"),
    
    {ok, _} = retest_sh:run("./rebar -C empty.config create "
                            "template=simplemod modid=foo -v",
                            [{env, [{"HOME", ".home_folder"}]}]),
    false = filelib:is_regular("deps/b/src/foo.erl"),
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
