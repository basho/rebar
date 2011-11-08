-module(t_import_config_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "rebar.config", "rebar.config"},
     {copy, "production.config", "production.config"},
     {create, "ebin/import_config.app", app(import_config, [import_config])}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    Ref = retest:sh("rebar -C production.config list-deps -v", [{async, true}]),
    {ok, _} = retest:sh_expect(Ref,
            "ERROR: Missing dependencies: \\[\\{dep,bad_name,"
            "foobar,\"\\.\\*\",undefined\\}\\]",
            [{newline, any}]),
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

