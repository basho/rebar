%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(ct4_rt).

-compile(export_all).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {create, "ebin/foo.app", app(foo)},
     {copy, "rebar.config", "rebar.config"},
     {copy, "foo.test.spec", "test/foo.test.spec"},
     {copy, "deps/bar.test.spec", "deps/bar.test.spec"},
     {copy, "deps/bar.test.spec", "baz.test.spec"},
     {copy, "foo_SUITE.erl", "test/foo_SUITE.erl"}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    Ref = retest:sh("./rebar compile ct -vvv", [async]),
    {ok, [[CTRunCmd]]} = retest:sh_expect(Ref, "^\"ct_run.*",
                                  [global, {capture, first, binary}]),
    {match, _} = re:run(CTRunCmd, "foo.test.spec", [global]),
    %% deps/bar.test.spec should be ignored by rebar_ct:collect_glob/3
    nomatch = re:run(CTRunCmd, "bar.test.spec", [global]),
    %% baz.test.spec should be also ignored by rebar_ct:collect_glob/3
    %% since we specified in rebar.config that we want to search for
    %% ct specs from the test dir
    nomatch = re:run(CTRunCmd, "baz.test.spec", [global]),
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
