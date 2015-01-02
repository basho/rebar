%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2015 Tomas Abrahamsson (tomas.abrahamsson@gmail.com)
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
-module(protobuffs_compile).
-export([scan_file/2]).

%% Simulate protobuffs compiling some proto files,
%% but generate only enough of what's needed for testing -- dummy stuff only.
scan_file(Proto, _Opts) ->
    ProtoBase = filename:basename(Proto, ".proto"),
    ModBase  = ProtoBase ++ "_pb",
    BeamDest = filename:join(get_beam_outdir(), ModBase ++ ".beam"),
    HrlDest  = filename:join(get_hrl_outdir(), ModBase ++ ".hrl"),
    ok = file:write_file(BeamDest, beam_text(ModBase)),
    ok = file:write_file(HrlDest, hrl_text(ModBase)).

beam_text(ModBase) ->
    Mod = list_to_atom(ModBase),
    Forms = [mk_attr(module, Mod)], % just a -module(...). line
    {ok, Mod, Bin} = compile:forms(Forms),
    Bin.

mk_attr(AttrName, AttrValue) ->
    erl_syntax:revert(
      erl_syntax:attribute(erl_syntax:atom(AttrName),
                           [erl_syntax:abstract(AttrValue)])).

hrl_text(ModBase) ->
    io_lib:format(
      lines(["-ifndef(~s_hrl).",
             "-define(~s_hrl, true).",
             "",
             "%% some record definitions would normally go here...",
             ""
             "-endif. %% ~s_hrl"]),
      [ModBase, ModBase, ModBase]).

get_beam_outdir() ->
    ".".

get_hrl_outdir() ->
    ".".

lines(Lines) ->
    lists:flatten([[L, $\n] || L <- Lines]).
