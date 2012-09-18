%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com)
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
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_edoc supports the following command:
%% <ul>
%%   <li>doc (essentially erl -noshell -run edoc_run application
%% "'$(&lt;app_name&gt;)'"
%% '"."' '[&lt;options&gt;]')</li>
%% </ul>
%% EDoc options can be given in the <code>edoc_opts</code> option in
%% <code>rebar.config</code>.
%% @copyright 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_edoc).

-export([doc/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

doc(Config, File) ->
    %% Save code path
    CodePath = setup_code_path(),

    %% Get the edoc_opts and app file info
    EDocConfig = rebar_config:get(Config, edoc_opts, []),
    {ok, Config1, AppName, _AppData} =
        rebar_app_utils:load_app_file(Config, File),

    %% Check whether to force the regeneration; if so, set 'new'
    Force = rebar_config:get_global(Config, force, "0") == "1",
    case Force of
        true ->
            ?INFO("Forcing regeneration of edocs for ~p\n", [AppName]),
            EDocOpts = [new | proplists:delete(new, EDocConfig)];
        false ->
            EDocOpts = EDocConfig
    end,

    %% Regenerate when forced or if any of the dependency files have changed
    ConfigName = rebar_config:source_file(Config),
    Regenerate = Force orelse needs_regen(EDocOpts, ConfigName),
    case Regenerate of
        true ->
            ?CONSOLE("Regenerating edocs for ~s\n", [AppName]),
            ?DEBUG("EDoc options: ~p\n", [EDocOpts]),
            ok = edoc:application(AppName, ".", EDocOpts);
        false ->
            ?INFO("Skipping regeneration of edocs for ~p\n", [AppName]),
            ok
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    {ok, Config1}.

%% ===================================================================
%% Internal functions
%% ===================================================================

setup_code_path() ->
    %% Setup code path prior to calling edoc so that edown, asciiedoc,
    %% and the like can work properly when generating their own
    %% documentation.
    CodePath = code:get_path(),
    true = code:add_patha(rebar_utils:ebin_dir()),
    CodePath.

-type path_spec() :: {'file', file:filename()} | file:filename().
-spec newer_file_exists(Paths::[path_spec()], OldFile::string()) -> boolean().
newer_file_exists(Paths, OldFile) ->
    OldModTime = filelib:last_modified(OldFile),
    ?DEBUG("Checking doc deps: ~p\n", [Paths]),

    ThrowIfNewer = fun(Fn, _Acc) ->
                           FModTime = filelib:last_modified(Fn),
                           Result = (FModTime > OldModTime) andalso
                               throw({newer_file_exists, {Fn, FModTime}}),
                           ?DEBUG("Skipped ~s\n", [Fn]),
                           Result
                   end,

    try
        lists:foldl(fun({file, F}, _) ->
                            ThrowIfNewer(F, false);
                       (P, _) ->
                            filelib:fold_files(P, ".*.(erl|hrl)", true,
                                               ThrowIfNewer, false)
                    end, undefined, Paths)
    catch
        throw:{newer_file_exists, {Filename, FMod}} ->
            ?DEBUG("~p \"~s\" is more recent than:\n", [FMod, Filename]),
            ?DEBUG("~p \"~s\", need to rebuild.\n", [OldModTime, OldFile]),
            true
    end.

%% Needs regen if any dependent file is changed since the last
%% edoc run. Dependent files are the erlang source files, the erlang header
%% files, the rebar config file, and the overview file, if it exists.
-spec needs_regen(proplists:proplist(), file:filename()) -> boolean().
needs_regen(EDocOpts, ConfigName) ->
    DocDir = proplists:get_value(dir, EDocOpts, "doc"),
    EDocInfoName = filename:join(DocDir, "edoc-info"),
    OverviewFile = proplists:get_value(overview, EDocOpts, "overview.edoc"),
    EDocOverviewName = filename:join(DocDir, OverviewFile),
    EDocOverviewPath = [{file, EDocOverviewName}],
    ConfigPath = [{file, ConfigName}],
    SrcPaths = proplists:get_value(source_path, EDocOpts, ["src"]),
    IncludePaths = proplists:get_value(source_path, EDocOpts, ["include"]),
    AllPaths = lists:append([EDocOverviewPath, ConfigPath,
                             IncludePaths, SrcPaths]),

    newer_file_exists(AllPaths, EDocInfoName).
