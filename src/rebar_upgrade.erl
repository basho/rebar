%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Joe Williams <joe@joetify.com>
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

-module(rebar_upgrade).

-include("rebar.hrl").

-export([upgrade/2]).

%% public api

upgrade(_Config, ReltoolFile) ->
    case rebar_config:get_global(oldreleasepath, false) of
        false ->
            ?ABORT("oldreleasepath=PATH is required to create upgrade package~n", []);
        OldVerPath ->
            {ok, {release, {NewReleaseName, NewReleaseVer}}} = 
                run_checks(OldVerPath, ReltoolFile),
            Release_NewVer = NewReleaseName ++ "_" ++ NewReleaseVer,
            setup(OldVerPath, NewReleaseName, NewReleaseVer, Release_NewVer),
            run_systools(Release_NewVer, NewReleaseName),
            cleanup(Release_NewVer)
    end.
  
%% internal api

run_checks(OldVerPath, ReltoolFile) ->
    true = release_path_check(OldVerPath),
    {ok, {release, {ReleaseName, ReleaseVersion}}} = get_release_name(ReltoolFile),
    true = release_path_check("./" ++ ReleaseName),
    {ok, {release, {NewReleaseName, NewReleaseVer}}} = get_release_version(ReleaseName, 
            "./" ++ ReleaseName),
    {ok, {release, {OldReleaseName, OldReleaseVer}}} = get_release_version(ReleaseName, 
            OldVerPath),
    true = release_name_check(NewReleaseName, 
            OldReleaseName, 
            "new and old .rel release names dont match"),
    true = release_name_check(ReleaseName, 
            NewReleaseName, 
            "reltool and .rel release names dont match"),
    true = new_old_release_version_check(NewReleaseVer, OldReleaseVer),
    true = reltool_release_version_check(ReleaseVersion, NewReleaseVer),
    {ok, {release, {NewReleaseName, NewReleaseVer}}}.

get_release_name(ReltoolFile) ->
    {ok, [{sys, ConfigList}, _]} = file:consult(ReltoolFile),
    %% expect the first rel in the proplist to be the one you want
    {rel, ReleaseName, ReleaseVersion, _} = proplists:lookup(rel, ConfigList),
    {ok, {release, {ReleaseName, ReleaseVersion}}}.
  
get_release_version(ReleaseName, Path) ->
    [RelFile] = filelib:wildcard(Path ++ "/releases/*/" ++ ReleaseName ++ ".rel"),
    [BinDir|_] = re:replace(RelFile, ReleaseName ++ "\\.rel", ""),
    {ok, [{release, {ReleaseName1, ReleaseVer}, _, _}]} = 
        file:consult(binary_to_list(BinDir) ++ ReleaseName ++ ".rel"),
    {ok, {release, {ReleaseName1, ReleaseVer}}}.

release_path_check(Path) ->
    case filelib:is_dir(Path) of
        true ->
            true;
        false ->
            ?ABORT("release directory doesn't exist (~p)~n", [Path])
    end.

reltool_release_version_check(Version1, Version2) when Version1 == Version2 ->
    true;
reltool_release_version_check(_, _) ->
    ?ABORT("reltool version and .rel versions dont match~n", []).

new_old_release_version_check(Version1, Version2) when Version1 /= Version2 ->
    true;
new_old_release_version_check(_, _) ->
    ?ABORT("new and old .rel contain the same version~n", []).

release_name_check(Name1, Name2, _) when Name1 == Name2 ->
    true;
release_name_check(_, _, Msg) ->
    ?ABORT("~p~n", [Msg]).

setup(OldVerPath, NewReleaseName, NewReleaseVer, Release_NewVer) ->
    NewRelPath = "./" ++ NewReleaseName,
    {ok, _} = file:copy(
        NewRelPath ++ "/releases/" ++ NewReleaseVer ++ "/" ++ NewReleaseName ++ ".rel", 
        "./" ++ Release_NewVer ++ ".rel"),
    ok = code:add_pathsa(filelib:wildcard(NewRelPath ++ "/*")),
    ok = code:add_pathsa(filelib:wildcard(NewRelPath ++ "/lib/*/ebin")),
    ok = code:add_pathsa(filelib:wildcard(OldVerPath ++ "/lib/*/ebin")),
    ok = code:add_pathsa(filelib:wildcard(OldVerPath ++ "/releases/*")).

run_systools(NewReleaseVer, ReleaseName) ->
    case systools:make_relup(NewReleaseVer, [ReleaseName], [ReleaseName], [silent]) of
        {error, _, _Message} ->
            ?ABORT("~p~n", [_Message]);
        _ ->
            ?DEBUG("relup created~n", []),
            case systools:make_script(NewReleaseVer, [silent]) of
                {error, _, _Message1} ->
                    ?ABORT("~p~n", [_Message1]);
                _ ->
                    ?DEBUG("script created~n", []),
                    case systools:make_tar(NewReleaseVer, [silent]) of
                        {error, _, _Message2} ->
                            ?ABORT("~p~n", [_Message2]);
                        _ ->
                            ?CONSOLE("~p upgrade package created~n", [ReleaseName])
                    end
            end
    end.
 
cleanup(Release_NewVer) ->
    ?DEBUG("removing files needed for building the upgrade~n", []),
    ok = file:delete("./" ++ Release_NewVer ++ ".rel"),
    ok = file:delete("./" ++ Release_NewVer ++ ".boot"),
    ok = file:delete("./" ++ Release_NewVer ++ ".script"),
    ok = file:delete("./relup").