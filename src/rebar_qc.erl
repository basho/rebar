%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Tuncer Ayaz
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
%% -------------------------------------------------------------------
-module(rebar_qc).

-export([qc/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================


qc(Config, _AppFile) ->
    QCOpts = rebar_config:get(Config, qc_opts, []),
    QC = select_qc_lib(QCOpts),
    ?DEBUG("Selected QC library: ~p~n", [QC]),
    run(Config, QC, QCOpts -- [{qc_lib, QC}]).

%% ===================================================================
%% Internal functions
%% ===================================================================

-define(PROPER_MOD, proper).
-define(EQC_MOD, eqc).

select_qc_lib(QCOpts) ->
    case proplists:get_value(qc_lib, QCOpts) of
        undefined ->
            detect_qc_lib();
        QC ->
            case code:ensure_loaded(QC) of
                {module, QC} ->
                    QC;
                {error, nofile} ->
                    ?ABORT("Configured QC library '~p' not available~n", [QC])
            end
    end.

detect_qc_lib() ->
    case code:ensure_loaded(?PROPER_MOD) of
        {module, ?PROPER_MOD} ->
            ?PROPER_MOD;
        {error, nofile} ->
            case code:ensure_loaded(?EQC_MOD) of
                {module, ?EQC_MOD} ->
                    ?EQC_MOD;
                {error, nofile} ->
                    ?ABORT("No QC library available~n", [])
            end
    end.

setup_codepath() ->
    CodePath = code:get_path(),
    true = code:add_patha(rebar_utils:test_dir()),
    true = code:add_patha(rebar_utils:ebin_dir()),
    CodePath.

run(Config, QC, QCOpts) ->
    ?DEBUG("QC Options: ~p~n", [QCOpts]),

    ok = filelib:ensure_dir(?TEST_DIR ++ "/foo"),
    CodePath = setup_codepath(),

    %% Compile erlang code to ?TEST_DIR, using a tweaked config
    %% with appropriate defines, and include all the test modules
    %% as well.
    {ok, _SrcErls} = rebar_erlc_compiler:test_compile(Config),

    case lists:flatten([qc_module(QC, QCOpts, M) || M <- find_prop_mods()]) of
        [] ->
            true = code:set_path(CodePath),
            ok;
        Errors ->
            ?ABORT("One or more QC properties didn't hold true:~n~p~n",
                   [Errors])
    end.

qc_module(QC=proper, QCOpts, M) -> QC:module(M, QCOpts);
qc_module(QC=eqc, QCOpts, M) -> QC:module(QCOpts, M).

find_prop_mods() ->
    Beams = rebar_utils:find_files(?TEST_DIR, ".*\\.beam\$"),
    [M || M <- [rebar_utils:erl_to_mod(Beam) || Beam <- Beams], has_prop(M)].

has_prop(Mod) ->
    lists:any(fun({F,_A}) -> lists:prefix("prop_", atom_to_list(F)) end,
              Mod:module_info(exports)).
