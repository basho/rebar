%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(ct_test_fails_rt).

-compile(export_all).


files() ->
    [{create, "ebin/a1.app", app(a1)},
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "app.config", "app.config"},
     {copy, "ct_test_fails.test.spec", "itest/ct_test_fails.test.spec"},
     {copy, "test_SUITE.erl", "itest/test_SUITE.erl"},
     {copy, "test2_SUITE.erl", "itest/test2_SUITE.erl"}].

run(Dir) ->
    ok = case catch retest:sh("./rebar compile ct -v 3") of
                {error, {stopped, _}} -> ok;
                _ -> expected_to_fail
              end.

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
