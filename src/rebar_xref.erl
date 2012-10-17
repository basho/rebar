%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% This module borrows heavily from http://github.com/etnt/exrefcheck project as
%% written by Torbjorn Tornkvist <tobbe@kreditor.se>, Daniel Luna and others.
%% -------------------------------------------------------------------
-module(rebar_xref).

-include("rebar.hrl").

-export([xref/2]).

%% ===================================================================
%% Public API
%% ===================================================================

xref(Config, AppFileName) ->
    {ok, [{application, _, AppFile}]} = file:consult(AppFileName),
    OrigPath = code:get_path(),

    %% Spin up xref
    {ok, _} = xref:start(xref),

    xref:set_default(xref, [{warnings,
                             rebar_config:get(Config, xref_warnings, false)},
                            {verbose, rebar_config:is_verbose(Config)}]),

    setup(Config, AppFile),

    %% Get list of xref checks we want to run
    XrefChecks = rebar_config:get(Config, xref_checks,
                                  [exports_not_used,
                                   deprecated_function_calls,
                                   undefined_function_calls]),

    AnalyseOk = analyse(Config, XrefChecks),

    %% Run custom queries
    QueryChecks = rebar_config:get(Config, xref_queries, []),
    QueryNoWarn = lists:all(fun check_query/1, QueryChecks),

    %% Restore the original code path
    true = code:set_path(OrigPath),

    %% Stop xref
    stopped = xref:stop(xref),

    case lists:member(false, [AnalyseOk, QueryNoWarn]) of
        true ->
            ?FAIL;
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

setup(Config, AppFile) ->
    Deps = add_deps(Config),
    OptsDeps = add_otp_deps(Config, AppFile),
    set_lib_path(Deps ++ OptsDeps),
    xref:add_directory(xref, rebar_utils:ebin_dir()).

add_deps(Config) ->
    Deps = rebar_config:get_local(Config, deps, []),
    DepsNames = lists:map(fun(T) when is_tuple(T) -> element(1, T);
                             (A) when is_atom(A) -> A
                          end,
                          Deps),
    ?DEBUG("Adding deps ~p~n", [DepsNames]),
    CWD = rebar_utils:get_cwd(),
    DepsDir = rebar_config:get_local(Config, deps_dir, "deps"),
    [code:add_path(filename:join([CWD, DepsDir, Dep, "ebin"])) ||
        Dep <- DepsNames],
    DepsNames.

add_otp_deps(Config, AppFile) ->
    %% fixme take from release files if exists.
    rebar_config:get_local(Config, xref_extra_otp_deps, []) ++
        proplists:get_value(applications, AppFile, []).

set_lib_path(Libs) ->
    ok = xref:set_library_path(xref, [code:lib_dir(Dep, ebin) || Dep <- Libs]).


analyse(Config, Analysis) ->
    lists:filter(fun(Analyse) -> do_analyse(Config, Analyse) end,
                 Analysis).

do_analyse(Config, Analyse) ->
    ?DEBUG("Running xref ~p~n", [Analyse]),
    {ok, Result0} = xref:analyse(xref, Analyse),
    Result1 = [Res || Res <- Result0,
                      filter_ignore_modules(Config, Res)],
    Result = filter_away_ignored(Analyse, Result1),
    ?DEBUG("~p result ~p~n", [Analyse, Result]),
    out_if_res(Analyse, Result).

filter_ignore_modules(Config, {MFA1, MFA2}) ->
    filter_ignore_modules(Config, MFA1) andalso
        filter_ignore_modules(Config, MFA2);
filter_ignore_modules(Config, {M, _F, _A}) ->
    not lists:member(M, ignore_modules(Config)).

ignore_modules(Config) ->
    rebar_config:get_local(Config, xref_ignore_modules, []) ++
        [erlang, init].

out_if_res(_Analyse, []) -> false;
out_if_res(Analyse, Result) ->
    ?CONSOLE("Errors in ~p:~n~s", [Analyse, pp_result(Result)]),
    true.

pp_result(List) when is_list(List) -> [[pp_result(Item), "\n"] || Item <- List];
pp_result({MFA1, MFA2}) -> [pp_mfa(MFA1), " -> ", pp_mfa(MFA2)];
pp_result(MFA) -> pp_mfa(MFA).

pp_mfa({M, F, A} = MFA) ->
    {Source, Line} = find_mfa_source(MFA),
    [atom_to_list(M),":", atom_to_list(F),"/", integer_to_list(A),
     " (", Source, ":", integer_to_list(Line), ")"].

check_query({Query, Value}) ->
    {ok, Answer} = xref:q(xref, Query),
    case Answer =:= Value of
        false ->
            ?CONSOLE("Query ~s~n answer ~p~n did not match ~p~n",
                     [Query, Answer, Value]),
            false;
        _     ->
            true
    end.

%%
%% Ignore behaviour functions, and explicitly marked functions
%%
filter_away_ignored(exports_not_used, UnusedExports) ->
    %% Functions can be ignored by using
    %% -ignore_xref([{F, A}, ...]).

    %% Setup a filter function that builds a list of behaviour callbacks and/or
    %% any functions marked to ignore. We then use this list to mask any
    %% functions marked as unused exports by xref
    F = fun(Mod) ->
                Attrs  = Mod:module_info(attributes),
                Ignore = keyall(ignore_xref, Attrs),
                Callbacks = [B:behaviour_info(callbacks)
                             || B <- keyall(behaviour, Attrs)],
                [{Mod, F, A} || {F, A} <- Ignore ++ lists:flatten(Callbacks)]
        end,
    AttrIgnore =
        lists:flatten(
          lists:map(F, lists:usort([M || {M, _, _} <- UnusedExports]))),
    [X || X <- UnusedExports, not lists:member(X, AttrIgnore)];
filter_away_ignored(_, UnusedExports) -> UnusedExports.


keyall(Key, List) ->
    lists:flatmap(fun({K, L}) when Key =:= K -> L; (_) -> [] end, List).

%%
%% Extract an element from a tuple, or undefined if N > tuple size
%%
safe_element(N, Tuple) ->
    case catch(element(N, Tuple)) of
        {'EXIT', {badarg, _}} ->
            undefined;
        Value ->
            Value
    end.


%%
%% Given a MFA, find the file and LOC where it's defined. Note that
%% xref doesn't work if there is no abstract_code, so we can avoid
%% being too paranoid here.
%%
find_mfa_source({M, F, A}) ->
    try
        {M, Bin, _} = code:get_object_code(M),
        AbstractCode = beam_lib:chunks(Bin, [abstract_code]),
        {ok, {M, [{abstract_code, {raw_abstract_v1, Code}}]}} = AbstractCode,
        do_find_mfa_source(Code, F, A)
    catch _:_ ->
            {"?", 0}
    end.

do_find_mfa_source(Code, F, A) ->
    %% Extract the original source filename from the abstract code
    [{attribute, 1, file, {Source, _}} | _] = Code,
    %% Extract the line number for a given function def
    Fn = [E || E <- Code,
               safe_element(1, E) == function,
               safe_element(3, E) == F,
               safe_element(4, E) == A],
    case Fn of
        [{function, Line, F, _, _}] -> {Source, Line};
        %% do not crash if functions are exported, even though they
        %% are not in the source.
        %% parameterized modules add new/1 and instance/1 for example.
        [] -> {Source, function_not_found}
    end.
