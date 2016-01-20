%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(thooks_rt).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     %% dummy lfe files
     {copy, "rebar.config", "rebar.config"},
     {copy, "fish.erl", "src/fish.erl"},
     {create, "ebin/fish.app", app(fish, [fish])}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar -v clean compile", [])),
    ensure_command_ran_only_once("preclean"),
    ensure_command_ran_only_once("precompile"),
    ensure_command_ran_only_once("postclean"),
    ensure_command_ran_only_once("postcompile"),
    ok.

ensure_command_ran_only_once(Command) ->
    File = Command ++ ".out",
    ?assert(filelib:is_regular(File)),
    %% ensure that this command only ran once (not for each module)
    {ok, Content} = file:read_file(File),
    %% echo behaves differently in windows and unix
    case os:type() of
        {win32, nt} ->
            ?assertEqual(Command ++ " \r\n", binary_to_list(Content));
        _ ->
            ?assertEqual(Command ++ "\n", binary_to_list(Content))
    end.

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
