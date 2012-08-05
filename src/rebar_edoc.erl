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
    EDocOpts = rebar_config:get(Config, edoc_opts, []),
    {ok, Config1, AppName, _AppData} =
        rebar_app_utils:load_app_file(Config, File),

    case needs_regen(EDocOpts) of
        true ->
            ?INFO("Regenerating edocs for ~p\n", [AppName]),
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
    true = code:add_patha(ebin_dir()),
    CodePath.

ebin_dir() ->
    filename:join(rebar_utils:get_cwd(), "ebin").

-spec file_modified_since(Filename::string(),
                          SinceTime::non_neg_integer()) -> true | no_return().
file_modified_since(Filename, SinceTime) ->
    FLast = filelib:last_modified(Filename),
    case FLast > SinceTime of
        true ->
            throw({newer_file_exists, {Filename, FLast}});
        false ->
            false
    end.


-spec newer_file_exists(Paths::[string()], LastMod::non_neg_integer()) ->
      false | no_return().
newer_file_exists(Paths, LastMod) ->
    CheckFile = fun(Fn, _Acc) ->
            file_modified_since(Fn, LastMod)
    end,
    lists:foldl(fun(P, _) ->
                filelib:fold_files(P, ".*.erl", true,
                                   CheckFile, false)
                end, undefined, Paths),
    false.

%% Needs regen if any dependent file is changed since the last
%% edoc run. Dependent files are the erlang source files,
%% and the overview file, if it exists.
-spec needs_regen(EDocOpts::edoc:proplist()) -> boolean().
needs_regen(EDocOpts) ->
    DocDir = proplists:get_value(dir, EDocOpts, "doc"),
    OverviewFile = proplists:get_value(overview, EDocOpts, "overview.edoc"),
    SrcPath = proplists:get_value(source_path, EDocOpts, ["src"]),

    %% Determine the age of the info file
    EDocInfoName = filename:join(DocDir, "edoc-info"),
    EDocInfoLastMod = filelib:last_modified(EDocInfoName),

    %% First check if the overview file has changed - no point in
    %% checking all the source files if we know we have to regen.
    %% Then, for each source directory, look for a more recent file than
    %% EDocInfoLastMod.
    EDocOverviewName = filename:join(DocDir, OverviewFile),
    try
        file_modified_since(EDocOverviewName, EDocInfoLastMod) orelse
        newer_file_exists(SrcPath, EDocInfoLastMod)
    catch
        throw: {newer_file_exists, {Filename, FMod}} ->
            ?DEBUG("~p is more recent than edoc-info: "
                "~120p > ~120p\n",
                [Filename, FMod, EDocInfoLastMod]),
            true
    end.

