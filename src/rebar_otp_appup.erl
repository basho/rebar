%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2015 Luis Rascao (luis.rascao@gmail.com)
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
-module(rebar_otp_appup).

-export([compile/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_config:config(), file:filename()) ->
    {'ok', rebar_config:config()}.
compile(Config, _AppFile) ->
    %% If we get an *.appup.src file, it needs to be pre-processed and
    %% written out as a ebin/*.appup file.
    Files = rebar_utils:find_files_by_ext("src", ".appup.src"),
    Targets = [filename:join("ebin",
                             filename:rootname(filename:basename(F)))
               || F <- Files],
    rebar_base_compiler:run(Config, [],
                            lists:zip(Files, Targets),
                            fun preprocess/3,
                            [{check_last_mod, true}]).

clean(Config, _AppFile) ->
    Files = rebar_utils:find_files_by_ext("ebin", ".appup"),
    rebar_file_utils:delete_each([Files]),
    {ok, Config}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    ?CONSOLE("Validate .appup.src file", []);
info(help, clean) ->
    ?CONSOLE("Delete .appup file if generated from .appup.src", []).

preprocess(SourceFile, TargetFile, _Config) ->
    %% Perform basic validation on the appup file
    %% i.e. if a consult succeeds and basic appup
    %% structure exists.
    case rebar_config:consult_file(SourceFile) of
        %% The .appup syntax is described in
        %% http://erlang.org/doc/man/appup.html.
        {ok, [{_Vsn, [_UpFromVsn], [_DownToVsn]} = AppUp]} ->
            case file:write_file(TargetFile,
                                 lists:flatten(io_lib:format("~p.", [AppUp]))) of
                {error, Reason} ->
                    ?ABORT("Failed writing to target file ~s due to ~s",
                        [TargetFile, Reason]);
                ok -> ok
            end;
        {error, Reason} ->
            ?ABORT("Failed to compile ~s: ~p~n", [SourceFile, Reason]);
        _ ->
            ?ABORT("Failed to compile ~s, not an appup~n", [SourceFile])
    end.
