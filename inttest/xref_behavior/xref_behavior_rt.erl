-module(xref_behavior_rt).

-export([files/0, run/1]).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "rebar.config", "rebar.config"},
     {copy, "xref_behavior.erl", "src/xref_behavior.erl"},
     {copy, "gen_xref_behavior.erl", "src/gen_xref_behavior.erl"},
     {create, "ebin/xref_behavior.app", app(xref_behavior,
                                            [xref_behavior,
                                             gen_xref_behavior])}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    {ok, _} = retest_sh:run("./rebar compile", []),
    {ok, _} = retest_sh:run("./rebar xref", []),
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

