%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Alfonso De Gregorio (adg@secYOUre.com), Dave Smith 
%% (dizzyd@dizzyd.com)
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
-module(rebar_idl_compiler).
-author('adg@secYOUre.com').

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config, filelib:wildcard("idl/*.idl"),
                            "idl", ".idl", "idl", ".erl",
                            fun compile_idl/3).

-spec clean(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
clean(Config, _AppFile) ->
    rebar_file_utils:delete_each(idl_generated_files("src", Config)),
    ok.

-spec compile_idl(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_idl(Source, Target, Config) ->
    ok = rebar_utils:ensure_dir(Target),
    Opts = rebar_config:get(Config, idl_opts, []),
    Backend = get_backend_name(Opts),
    Outdir = proplists:get_value(outdir, Opts, "src/backends/" ++ Backend),
    ok = rebar_utils:ensure_dir(Outdir),
    Opts2 = [{outdir, Outdir} ] ++ Opts,
    case ic:gen(Source, Opts2) of
        ok ->
            ok;
        {error, _Reason} ->
            ?FAIL
    end.

idl_generated_files(SrcDir, Config) ->
    Opts = rebar_config:get(Config, idl_opts, []),
    Backend = get_backend_name(Opts),
    Outdir = proplists:get_value(outdir, Opts, SrcDir ++ "/backends/" ++ Backend),
    rebar_utils:find_files(Outdir, "^.*\$").

get_backend_name(Opts) ->
        atom_to_list(proplists:get_value(be, Opts, erl_corba)).
