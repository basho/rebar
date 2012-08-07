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
%% -------------------------------------------------------------------
-module(rebar_file_utils).

-export([rm_rf/1,
         cp_r/2,
         mv/2,
         delete_each/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Remove files and directories.
%% Target is a single filename, directoryname or wildcard expression.
-spec rm_rf(string()) -> 'ok'.
rm_rf(Target) ->
    Filelist = filelib:wildcard(Target),
    Dirs = [F || F <- Filelist, filelib:is_dir(F)],
    Files = Filelist -- Dirs,
    ok = delete_each(Files),
    case os:type() of
        {unix, _} ->
            ok = delete_each_dir_unix(Dirs);
        {win32, _} ->
            ok = delete_each_dir_win32(Dirs),
            ok
    end.

-spec cp_r(list(string()), file:filename()) -> 'ok'.
cp_r([], _Dest) ->
    ok;
cp_r(Sources, Dest) ->
    case os:type() of
        {unix, _} ->
            ok = cp_r_unix(Sources, Dest);
        {win32, _} ->
            lists:foreach(fun(Src) -> ok = cp_r_win32(Src,Dest) end, Sources),
            ok
    end.

-spec mv(string(), file:filename()) -> 'ok'.
mv(Source, Dest) ->
    case os:type() of
        {unix, _} ->
            EscSource = escape_spaces(Source),
            EscDest = escape_spaces(Dest),
            {ok, []} = rebar_utils:sh(?FMT("mv ~s ~s", [EscSource, EscDest]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
        {win32, _} ->
            {ok, R} = rebar_utils:sh(
                        ?FMT("move /y \"~s\" \"~s\" 1> nul",
                             [filename:nativename(Source),
                              filename:nativename(Dest)]),
                        [{use_stdout, false}, return_on_error]),
            case R of
                [] ->
                    ok;
                _ ->
                    {error, lists:flatten(
                              io_lib:format("Failed to move ~s to ~s~n",
                                            [Source, Dest]))}
            end
    end.

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            delete_each(Rest);
        {error, enoent} ->
            delete_each(Rest);
        {error, Reason} ->
            ?ERROR("Failed to delete file ~s: ~p\n", [File, Reason]),
            ?FAIL
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

delete_each_dir_win32([]) -> ok;
delete_each_dir_win32([Dir | Rest]) ->
    {ok, []} = rebar_utils:sh(?FMT("rd /q /s \"~s\"",
                                   [filename:nativename(Dir)]),
                              [{use_stdout, false}, return_on_error]),
    delete_each_dir_win32(Rest).

xcopy_win32(Source,Dest)->
    {ok, R} = rebar_utils:sh(
                ?FMT("xcopy \"~s\" \"~s\" /q /y /e 2> nul",
                     [filename:nativename(Source), filename:nativename(Dest)]),
                [{use_stdout, false}, return_on_error]),
    case length(R) > 0 of
        %% when xcopy fails, stdout is empty and and error message is printed
        %% to stderr (which is redirected to nul)
        true -> ok;
        false ->
            {error, lists:flatten(
                      io_lib:format("Failed to xcopy from ~s to ~s~n",
                                    [Source, Dest]))}
    end.

cp_r_win32({true, SourceDir}, {true, DestDir}) ->
    %% from directory to directory
    SourceBase = filename:basename(SourceDir),
    ok = case file:make_dir(filename:join(DestDir, SourceBase)) of
             {error, eexist} -> ok;
             Other -> Other
         end,
    ok = xcopy_win32(SourceDir, filename:join(DestDir, SourceBase));
cp_r_win32({false, Source} = S,{true, DestDir}) ->
    %% from file to directory
    cp_r_win32(S, {false, filename:join(DestDir, filename:basename(Source))});
cp_r_win32({false, Source},{false, Dest}) ->
    %% from file to file
    {ok,_} = file:copy(Source, Dest),
    ok;
cp_r_win32({true, SourceDir}, {false, DestDir}) ->
    case filelib:is_regular(DestDir) of
        true ->
            %% From directory to file? This shouldn't happen
            {error, lists:flatten(
                      io_lib:format("Cannot copy dir (~p) to file (~p)\n",
                                    [SourceDir, DestDir]))};
        false ->
            %% Specifying a target directory that doesn't currently exist.
            %% So let's attempt to create this directory
            case filelib:ensure_dir(filename:join(DestDir, "dummy")) of
                ok ->
                    ok = xcopy_win32(SourceDir, DestDir);
                {error, Reason} ->
                    {error, lists:flatten(
                              io_lib:format("Unable to create dir ~p: ~p\n",
                                            [DestDir, Reason]))}
            end
    end;
cp_r_win32(Source,Dest) ->
    Dst = {filelib:is_dir(Dest), Dest},
    lists:foreach(fun(Src) ->
                          ok = cp_r_win32({filelib:is_dir(Src), Src}, Dst)
                  end, filelib:wildcard(Source)),
    ok.

cp_r_unix([], _Dest) ->
    ok;
cp_r_unix([Source|Rest], Dest) ->
    Filelist = filelib:wildcard(Source),
    Dirs = [F || F <- Filelist, filelib:is_dir(F)],
    Files = Filelist -- Dirs,
    lists:foreach(fun(S) ->
                inner_copy_file(S, Dest)
        end, Files),
    lists:foreach(fun(S) -> ok = inner_copy(S, Dest) end, Dirs),
    ok = cp_r_unix(Rest, Dest).

inner_copy(Source, Dest) ->
    case filelib:is_dir(Source) of
        true ->
            NewDest = filename:join([Dest, filename:basename(Source)]),
            ok = case file:make_dir(NewDest) of
                {error, eexist} -> ok;
                Other -> Other
            end,
            {ok, Filelist} = file:list_dir(Source),
            ToCopy = [filename:join([Source, X]) || X <- Filelist],
            lists:foreach(fun(T) -> ok = inner_copy(T, NewDest) end, ToCopy),
            ok;
        false ->
            ok = inner_copy_file(Source, Dest),
            ok
    end.

inner_copy_file(Source, Dest) ->
    DestFile = case filelib:is_dir(Dest) of
        true ->
            filename:join([Dest, filename:basename(Source)]);
        false ->
            Dest
    end,
    {ok, _} = file:copy(Source, DestFile),
    ok.

delete_each_dir_unix([]) -> ok;
delete_each_dir_unix([Dir | Rest]) ->
    {ok, Filelist} = file:list_dir(Dir),
    ToDelete = [filename:join(Dir, F) || F <- Filelist],
    SubDirs = [F || F <- ToDelete, filelib:is_dir(F)],
    Files = ToDelete -- SubDirs,
    ok = delete_each(Files),
    ok = delete_each_dir_unix(SubDirs),
    ok = file:del_dir(Dir),
    delete_each_dir_unix(Rest).

escape_spaces(Str) ->
    re:replace(Str, " ", "\\\\ ", [global, {return, list}]).
