%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Sean Cribbs (sean@basho.com)
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

%% The rebar_reia_compiler module is a plugin for rebar that compiles
%% Reia source files.  By default, it compiles all src/*.re to ebin/*.reb
%%
%% Configuration options should be placed in rebar.config under
%% reia_opts.  Available options include:
%%
%% src_dir: where to find the peg files to compile.
%%           "src" by default
%% out_dir: where to put the compiled files.
%%          "ebin" by default
%% module_ext: characters to append to the module's name.
%%             ".reb" by default
%% source_ext: extension of Reia source files.
%%             ".re" by default
-module(rebar_reia_compiler).

-export([compile/2]).

-include("rebar.hrl").

%% ============================================================================
%% Public API
%% ============================================================================

compile(Config, _AppFile) ->
  ReiaOpts = reia_opts(Config),
  rebar_base_compiler:run(Config, [],
                          option(src_dir, ReiaOpts),
                          option(source_ext, ReiaOpts),
                          option(out_dir, ReiaOpts),
                          option(module_ext, ReiaOpts),
                          fun compile_reia/3).

%% ============================================================================
%% Private API
%% ============================================================================

reia_opts(Config) ->
  rebar_config:get(Config, reia_opts, []).

option(Opt, Options) ->
  proplists:get_value(Opt, Options, default(Opt)).

default(src_dir) -> "src";
default(out_dir) -> "ebin";
default(module_ext) -> ".reb";
default(source_ext) -> ".re".

compile_reia(Source, Target, Config) ->
    filelib:ensure_dir(Target),
    case code:which(reia_internal) of
        non_existing ->
            ?CONSOLE(
               <<"~n=====================================================~n"
                 " You need to install Reia to compile Reia source files.~n"
                 " Download the latest tarball release from github:~n"
                 "    https://github.com/tarcieri/reia~n"
                 " and install it into your Erlang library dir~n"
                 " or add it as a dependency in rebar.config~n"
                 "======================================================~n~n">>, []),
            ?FAIL;
        _ ->
            do_compile(Source, Target, Config)
    end.

do_compile(Source, Target, _Config) ->
    case catch reia_internal:compile(Source, Target) of
        ok ->
          ok;
        Reason ->
            ?CONSOLE("Compiling ~s failed:~n  ~p~n",
                     [Source, Reason]),
            ?FAIL
    end.

needs_compile(Source, Target, _Config) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source).
