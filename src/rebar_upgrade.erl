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
            ok = cleanup(Release_NewVer)
    end.
  
%% internal api

run_checks(OldVerPath, ReltoolFile) ->
    true = release_path_check(OldVerPath),
    {ok, {release, {ReleaseName, ReleaseVersion}}} = 
        get_release_name(ReltoolFile),
    true = release_path_check("./" ++ ReleaseName),
    {ok, {release, {NewReleaseName, NewReleaseVer}}} = 
        get_release_version(ReleaseName, "./" ++ ReleaseName),
    {ok, {release, {OldReleaseName, OldReleaseVer}}} = 
        get_release_version(ReleaseName, OldVerPath),
    true = 
        release_name_check(ReleaseName, NewReleaseName, OldReleaseName),
    true = 
        release_version_check(ReleaseVersion, NewReleaseVer, OldReleaseVer),
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

release_version_check(ReleaseVersion, NewReleaseVer, OldReleaseVer) ->
    case ReleaseVersion == NewReleaseVer of
        true ->
            case NewReleaseVer == OldReleaseVer of
                true ->
                    ?ABORT("new and old .rel contain the same version~n", []);
                false ->
                    true
            end;
        false ->
            ?ABORT("reltool version and .rel versions dont match~n", [])
    end.

release_name_check(ReleaseVersion, NewReleaseVer, OldReleaseVer) ->
    case ReleaseVersion == NewReleaseVer of
        true ->
            case NewReleaseVer == OldReleaseVer of
                true ->
                    true;
                false ->
                    ?ABORT("new and old .rel release names dont match~n", [])
            end;
        false ->
            ?ABORT("reltool and .rel release names dont match~n", [])
    end.

setup(OldVerPath, NewReleaseName, NewReleaseVer, Release_NewVer) ->
    NewRelPath = "./" ++ NewReleaseName,
    file:copy(
        NewRelPath ++ "/releases/" ++ NewReleaseVer ++ "/" ++ NewReleaseName ++ ".rel", 
        "./" ++ Release_NewVer ++ ".rel"),
    code:add_pathsa(filelib:wildcard(NewRelPath ++ "/*")),
    code:add_pathsa(filelib:wildcard(NewRelPath ++ "/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard(OldVerPath ++ "/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard(OldVerPath ++ "/releases/*")).

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
    file:delete("./" ++ Release_NewVer ++ ".rel"),
    file:delete("./" ++ Release_NewVer ++ ".boot"),
    file:delete("./" ++ Release_NewVer ++ ".script"),
    file:delete("./relup"),
    ok.