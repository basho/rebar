%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_dia_compiler).

-export([compile/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_config:config(), file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    DiaOpts = rebar_config:get(Config, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),
    DiaFiles = filelib:wildcard("dia/*.dia"),
    code:add_pathsz(["ebin" | IncludeEbin]),
    FileSequence = case rebar_config:get(Config, dia_first_files, []) of
        [] ->
            DiaFiles;
        CompileFirst ->
            CompileFirst ++
            [F || F <- DiaFiles, not lists:member(F, CompileFirst)]
    end,
    rebar_base_compiler:run(Config, FileSequence,
                            "dia", ".dia", "src", ".erl",
                            fun compile_dia/3).

-spec clean(rebar_config:config(), file:filename()) -> 'ok'.
clean(Config, _AppFile) ->
    DiaOpts = rebar_config:get(Config, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),
    code:add_pathsz(["ebin" | IncludeEbin]),
    GeneratedFiles = dia_generated_files("dia", "src", "include"),
    ok = rebar_file_utils:delete_each(GeneratedFiles),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Build Diameter (*.dia) sources");
info(help, clean) ->
    info_help("Delete generated Diameter files").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  {dia_opts, []} (options from diameter_make:codec/2 supported with~n"
       "                  exception of inherits)~n"
       "  {dia_first_files, []} (files in sequence to compile first)~n",
       [Description]).

-spec compile_dia(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_dia(Source, Target, Config) ->
    ok = filelib:ensure_dir(Target),
    ok = filelib:ensure_dir(filename:join("include", "dummy.hrl")),
    ok = filelib:ensure_dir(filename:join("ebin", "dummy.beam")),
    true = code:add_path(filename:absname("ebin")),
    Opts = [{outdir, "src"}] ++ rebar_config:get(Config, dia_opts, []),
    case diameter_dict_util:parse({path, Source}, []) of
        {ok, Spec} ->
            FileName = dia_filename(Source, Spec),
            _ = diameter_codegen:from_dict(FileName, Spec, Opts, erl),
            _ = diameter_codegen:from_dict(FileName, Spec, Opts, hrl),
            HrlFile = filename:join("src", FileName ++ ".hrl"),
            ErlFile = filename:join("src", FileName ++ ".erl"),
            ErlCOpts = [{outdir, "ebin"}] ++
                        rebar_config:get(Config, erl_opts, []),
            _ = compile:file(ErlFile, ErlCOpts),
            case filelib:is_regular(HrlFile) of
                true ->
                    ok = rebar_file_utils:mv(HrlFile, "include");
                false ->
                    ok
            end;
        {error, Reason} ->
            ?ABORT(
                "Compiling ~s failed: ~s~n",
                [Source, diameter_dict_util:format_error(Reason)]
            )
    end.

dia_generated_files(DiaDir, SrcDir, IncDir) ->
    F = fun(File, Acc) ->
            case catch diameter_dict_util:parse({path, File}, []) of
                {ok, Spec} ->
                    FileName = dia_filename(File, Spec),
                    [
                        filename:join([IncDir, FileName ++ ".hrl"]) |
                        filelib:wildcard(
                            filename:join([SrcDir, FileName ++ ".*"])
                        )
                    ] ++ Acc;
                _ ->
                    Acc
            end
    end,
    lists:foldl(F, [], filelib:wildcard(filename:join([DiaDir, "*.dia"]))).

dia_filename(File, Spec) ->
    case proplists:get_value(name, Spec) of
        undefined ->
            filename:rootname(filename:basename(File));
        Name ->
            Name
    end.
