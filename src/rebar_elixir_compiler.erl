%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com),
%%                    Tim Dysinger (tim@dysinger.net)
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

-module(rebar_elixir_compiler).

-export([compile/2, clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(_Config, _AppFile) ->
    case rebar_utils:find_files("src", ".*\\.ex$") of
        [] ->
            ok;
        FoundFiles ->
            application:start(elixir),
            OutDir = "ebin",
            case code:which(elixir) of
                non_existing ->
                    ?ERROR("~n"
                        "*** Missing Elixir compiler ***~n"
                        "  You must do one of the following:~n"
                        "    a) Install Elixir globally in your erl libs~n"
                        "    b) Add Elixir as a dep for your project, eg:~n"
                        "      {deps, [{elixir, \"0.7.2\",~n"
                        "        {git, \"git://github.com/elixir-lang/elixir\",~n"
                        "         {tag, \"v0.7.2\"}}}]}.~n"
                        "      {lib_dirs, [\"deps/elixir/lib\"]}.~n" 
                        "~n", []),
                    ?FAIL;
                _ ->
                    [_|_] = 'Elixir-Kernel-ParallelCompiler':files_to_path(
                        lists:map(fun list_to_binary/1, FoundFiles), list_to_binary(OutDir)),
                    ok
            end
    end.

clean(_Config, _AppFile) ->
    delete_each(rebar_utils:find_files("ebin", "^Elixir-.*\\.beam$")).

%% ===================================================================
%% Internal functions
%% ===================================================================

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            ?ERROR("Failed to delete ~s: ~p\n", [File, Reason])
    end,
    delete_each(Rest).
