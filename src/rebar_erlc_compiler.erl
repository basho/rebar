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
-module(rebar_erlc_compiler).

-export([compile/2,
         clean/2]).

-export([doterl_compile/2,
         doterl_compile/3]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% Supported configuration variables:
%%
%% * erl_opts - Erlang list of options passed to compile:file/2
%%              It is also possible to specify platform specific
%%              options by specifying a pair or a triplet where the
%%              first string is a regex that is checked against the
%%              string
%%
%%                OtpRelease ++ "-" ++ SysArch ++ "-" ++ Words.
%%
%%              where
%%
%%                OtpRelease = erlang:system_info(otp_release).
%%                SysArch = erlang:system_info(system_architecture).
%%                Words = integer_to_list(8 * erlang:system_info(wordsize)).
%%
%%              E.g. to define HAVE_SENDFILE only on systems with
%%              sendfile(), to define BACKLOG on Linux/FreeBSD as 128,
%%              and to define 'old_inets' for R13 OTP release do:
%%
%%              {erl_opts, [{platform_define,
%%                           "(linux|solaris|freebsd|darwin)",
%%                           'HAVE_SENDFILE'},
%%                          {platform_define, "(linux|freebsd)",
%%                           'BACKLOG', 128},
%%                          {platform_define, "R13",
%%                           'old_inets'}]}.
%%

-spec compile(Config::#config{}, AppFile::string()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config,
                            check_files(rebar_config:get_local(
                                          Config, xrl_first_files, [])),
                            "src", ".xrl", "src", ".erl",
                            fun compile_xrl/3),
    rebar_base_compiler:run(Config,
                            check_files(rebar_config:get_local(
                                          Config, yrl_first_files, [])),
                            "src", ".yrl", "src", ".erl",
                            fun compile_yrl/3),
    doterl_compile(Config, "ebin"),
    rebar_base_compiler:run(Config,
                            check_files(rebar_config:get_local(
                                          Config, mib_first_files, [])),
                            "mibs", ".mib", "priv/mibs", ".bin",
                            fun compile_mib/3).

-spec clean(Config::#config{}, AppFile::string()) -> 'ok'.
clean(_Config, _AppFile) ->
    lists:foreach(fun(F) -> ok = rebar_file_utils:rm_rf(F) end,
                  ["ebin/*.beam", "priv/mibs/*.bin"]),

    YrlFiles = rebar_utils:find_files("src", "^.*\\.[x|y]rl\$"),
    rebar_file_utils:delete_each(
      [ binary_to_list(iolist_to_binary(re:replace(F, "\\.[x|y]rl$", ".erl")))
        || F <- YrlFiles  ]),

    %% Erlang compilation is recursive, so it's possible that we have a nested
    %% directory structure in ebin with .beam files within. As such, we want
    %% to scan whatever is left in the ebin/ directory for sub-dirs which
    %% satisfy our criteria.
    BeamFiles = rebar_utils:find_files("ebin", "^.*\\.beam\$"),
    rebar_file_utils:delete_each(BeamFiles),
    lists:foreach(fun(Dir) -> delete_dir(Dir, dirs(Dir)) end, dirs("ebin")),
    ok.


%% ===================================================================
%% .erl Compilation API (externally used by only eunit)
%% ===================================================================

-spec doterl_compile(Config::#config{}, OutDir::string()) -> 'ok'.
doterl_compile(Config, OutDir) ->
    doterl_compile(Config, OutDir, []).

doterl_compile(Config, OutDir, MoreSources) ->
    FirstErls = rebar_config:get_list(Config, erl_first_files, []),
    RawErlOpts = filter_defines(rebar_config:get(Config, erl_opts, []), []),
    ErlOpts =
        case rebar_config:get_global(debug_info, "0") of
            "0" ->
                RawErlOpts;
            _ ->
                [debug_info|RawErlOpts]
        end,
    ?DEBUG("erl_opts ~p~n",[ErlOpts]),
    %% Support the src_dirs option allowing multiple directories to
    %% contain erlang source. This might be used, for example, should
    %% eunit tests be separated from the core application source.
    SrcDirs = src_dirs(proplists:append_values(src_dirs, ErlOpts)),
    RestErls  = [Source || Source <- gather_src(SrcDirs, []) ++ MoreSources,
                           not lists:member(Source, FirstErls)],

    %% Split RestErls so that parse_transforms and behaviours are instead added
    %% to erl_first_files, parse transforms first.
    %% This should probably be somewhat combined with inspect_epp
    [ParseTransforms, Behaviours, OtherErls] =
        lists:foldl(fun(F, [A, B, C]) ->
                            case compile_priority(F) of
                                parse_transform ->
                                    [[F | A], B, C];
                                behaviour ->
                                    [A, [F | B], C];
                                _ ->
                                    [A, B, [F | C]]
                            end
                    end, [[], [], []], RestErls),

    NewFirstErls = FirstErls ++ ParseTransforms ++ Behaviours,

    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join("ebin", "dummy.beam")),
    CurrPath = code:get_path(),
    true = code:add_path(filename:absname("ebin")),
    rebar_base_compiler:run(Config, NewFirstErls, OtherErls,
                            fun(S, C) ->
                                    internal_erl_compile(S, C, OutDir, ErlOpts)
                            end),
    true = code:set_path(CurrPath),
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

-spec include_path(Source::string(), Config::#config{}) -> [string(), ...].
include_path(Source, Config) ->
    ErlOpts = rebar_config:get(Config, erl_opts, []),
    ["include", filename:dirname(Source)]
        ++ proplists:get_all_values(i, ErlOpts).

-spec inspect(Source::string(),
              IncludePath::[string(),...]) -> {string(), [string()]}.
inspect(Source, IncludePath) ->
    ModuleDefault = filename:basename(Source, ".erl"),
    case epp:open(Source, IncludePath) of
        {ok, Epp} ->
            inspect_epp(Epp, Source, ModuleDefault, []);
        {error, Reason} ->
            ?DEBUG("Failed to inspect ~s: ~p\n", [Source, Reason]),
            {ModuleDefault, []}
    end.

-spec inspect_epp(Epp::pid(), Source::string(), Module::string(),
                  Includes::[string()]) -> {string(), [string()]}.
inspect_epp(Epp, Source, Module, Includes) ->
    case epp:parse_erl_form(Epp) of
        {ok, {attribute, _, module, ModInfo}} ->
            case ModInfo of
                %% Typical module name, single atom
                ActualModule when is_atom(ActualModule) ->
                    ActualModuleStr = atom_to_list(ActualModule);
                %% Packag-ized module name, list of atoms
                ActualModule when is_list(ActualModule) ->
                    ActualModuleStr = string:join([atom_to_list(P) ||
                                                      P <- ActualModule], ".");
                %% Parameterized module name, single atom
                {ActualModule, _} when is_atom(ActualModule) ->
                    ActualModuleStr = atom_to_list(ActualModule);
                %% Parameterized and packagized module name, list of atoms
                {ActualModule, _} when is_list(ActualModule) ->
                    ActualModuleStr = string:join([atom_to_list(P) ||
                                                      P <- ActualModule], ".")
            end,
            inspect_epp(Epp, Source, ActualModuleStr, Includes);
        {ok, {attribute, 1, file, {Module, 1}}} ->
            inspect_epp(Epp, Source, Module, Includes);
        {ok, {attribute, 1, file, {Source, 1}}} ->
            inspect_epp(Epp, Source, Module, Includes);
        {ok, {attribute, 1, file, {IncFile, 1}}} ->
            inspect_epp(Epp, Source, Module, [IncFile | Includes]);
        {eof, _} ->
            epp:close(Epp),
            {Module, Includes};
        _ ->
            inspect_epp(Epp, Source, Module, Includes)
    end.

-spec needs_compile(Source::string(), Target::string(),
                    Hrls::[string()]) -> boolean().
needs_compile(Source, Target, Hrls) ->
    TargetLastMod = filelib:last_modified(Target),
    lists:any(fun(I) -> TargetLastMod < filelib:last_modified(I) end,
              [Source] ++ Hrls).

-spec internal_erl_compile(Source::string(), Config::#config{},
                           Outdir::string(),
                           ErlOpts::list()) -> 'ok' | 'skipped'.
internal_erl_compile(Source, Config, Outdir, ErlOpts) ->
    %% Determine the target name and includes list by inspecting the source file
    {Module, Hrls} = inspect(Source, include_path(Source, Config)),

    %% Construct the target filename
    Target = filename:join([Outdir | string:tokens(Module, ".")]) ++ ".beam",
    ok = filelib:ensure_dir(Target),

    %% If the file needs compilation, based on last mod date of includes or
    %% the target,
    case needs_compile(Source, Target, Hrls) of
        true ->
            Opts = [{outdir, filename:dirname(Target)}] ++
                ErlOpts ++ [{i, "include"}, report, return],
            case compile:file(Source, Opts) of
                {ok, _, []} ->
                    ok;
                {ok, _, _Warnings} ->
                    %% We got at least one warning -- if fail_on_warning
                    %% is in options, fail
                    case lists:member(fail_on_warning, Opts) of
                        true ->
                            %% remove target to prevent overlooking this failure
                            ok = file:delete(Target),
                            ?FAIL;
                        false ->
                            ok
                    end;
                _ ->
                    ?FAIL
            end;
        false ->
            skipped
    end.

-spec compile_mib(Source::string(), Target::string(),
                  Config::#config{}) -> 'ok'.
compile_mib(Source, Target, Config) ->
    ok = rebar_utils:ensure_dir(Target),
    Opts = [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++
        rebar_config:get(Config, mib_opts, []),
    case snmpc:compile(Source, Opts) of
        {ok, _} ->
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.

-spec compile_xrl(Source::string(), Target::string(),
                  Config::#config{}) -> 'ok'.
compile_xrl(Source, Target, Config) ->
    Opts = [{scannerfile, Target}, {return, true}
            |rebar_config:get(Config, xrl_opts, [])],
    compile_xrl_yrl(Source, Target, Opts, leex).

-spec compile_yrl(Source::string(), Target::string(),
                  Config::#config{}) -> 'ok'.
compile_yrl(Source, Target, Config) ->
    Opts = [{parserfile, Target}, {return, true}
            |rebar_config:get(Config, yrl_opts, [])],
    compile_xrl_yrl(Source, Target, Opts, yecc).

-spec compile_xrl_yrl(Source::string(), Target::string(), Opts::list(),
                      Mod::atom()) -> 'ok'.
compile_xrl_yrl(Source, Target, Opts, Mod) ->
    case needs_compile(Source, Target, []) of
        true ->
            case Mod:file(Source, Opts) of
                {ok, _, []} ->
                    ok;
                {ok, _, _Warnings} ->
                    case lists:member(fail_on_warning, Opts) of
                        true ->
                            ?FAIL;
                        false ->
                            ok
                    end;
                _X ->
                    ?FAIL
            end;
        false ->
            skipped
    end.

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(Rest, Srcs ++ rebar_utils:find_files(Dir, ".*\\.erl\$")).

-spec src_dirs(SrcDirs::[string()]) -> [string(),...].
src_dirs([]) ->
    ["src"];
src_dirs(SrcDirs) ->
    SrcDirs ++ src_dirs([]).

-spec dirs(Dir::string()) -> [string()].
dirs(Dir) ->
    [F || F <- filelib:wildcard(filename:join([Dir, "*"])), filelib:is_dir(F)].

-spec delete_dir(Dir::string(),
                 Subdirs::[string()]) -> 'ok' | {'error', atom()}.
delete_dir(Dir, []) ->
    file:del_dir(Dir);
delete_dir(Dir, Subdirs) ->
    lists:foreach(fun(D) -> delete_dir(D, dirs(D)) end, Subdirs),
    file:del_dir(Dir).

-spec compile_priority(File::string()) -> 'normal' | 'behaviour' |
                                          'parse_transform'.
compile_priority(File) ->
    case epp_dodger:parse_file(File) of
        {error, _} ->
            normal; % couldn't parse the file, default priority
        {ok, Trees} ->
            F2 = fun({tree,arity_qualifier,_,
                      {arity_qualifier,{tree,atom,_,behaviour_info},
                       {tree,integer,_,1}}}, _) ->
                         behaviour;
                    ({tree,arity_qualifier,_,
                      {arity_qualifier,{tree,atom,_,parse_transform},
                       {tree,integer,_,2}}}, _) ->
                         parse_transform;
                    (_, Acc) ->
                         Acc
                 end,

            F = fun({tree, attribute, _,
                     {attribute, {tree, atom, _, export},
                      [{tree, list, _, {list, List, none}}]}}, Acc) ->
                        lists:foldl(F2, Acc, List);
                   (_, Acc) ->
                        Acc
                end,

            lists:foldl(F, normal, Trees)
    end.

%%
%% Filter a list of erl_opts platform_define options such that only
%% those which match the provided architecture regex are returned.
%%
-spec filter_defines(ErlOpts::list(),Acc::list()) -> list().
filter_defines([], Acc) ->
    lists:reverse(Acc);
filter_defines([{platform_define, ArchRegex, Key} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([{platform_define, ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key, Value} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([Opt | Rest], Acc) ->
    filter_defines(Rest, [Opt | Acc]).

%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files(FileList::[string()]) -> [string()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> ?ABORT("File ~p is missing, aborting\n", [File]);
        true -> File
    end.
