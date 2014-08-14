%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(app_src_script_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "../../rebar", "rebar"},
     {create, "src/app_src.app.src.script", app_script(app_src)}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    {ok, [_Pid|Output]} = retest:sh("./rebar compile -vv",
                                    [{async, false}]),

    Regexp = "DEBUG: Evaluating config script .*/app_src\.app\.src\.script.*",
    ?assertEqual(true, has_line(Output, Regexp)),
    retest_log:log(debug, "Evaluated .app.src.script~n", []),

    %% check that ebin/app_src.app exists
    ?assertMatch(true, filelib:is_regular("ebin/app_src.app")),
    retest_log:log(debug, "Generated ebin/.app~n", []),

    %% check that ebin/.app has vsn="2"
    {ok, Bin} = file:read_file("ebin/app_src.app"),
    Str = binary_to_list(Bin),
    ?assertMatch({match, _}, re:run(Str, "{vsn, *\"2\"}")),
    retest_log:log(debug, "Variable replacement in .app is ok.~n", []),

    ok.

has_line([], _RE) ->
    false;
has_line([L|T], RE) ->
    case re:run(L, RE, []) of
        {match, _Captured} ->
            true;
        match ->
            true;
        nomatch ->
            has_line(T, RE)
    end.

%%
%% Generate the contents of a simple .app.src.script file
%%
app_script(Name) ->
    "Vsn=\"2\".\n" ++
        "{application, " ++ atom_to_list(Name) ++ ",
           [{vsn, Vsn},
            {modules, []},
            {registered, []},
            {applications, [kernel, stdlib]}]}.\n".
