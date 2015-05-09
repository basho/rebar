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
-module(gpb_compile).
-export([file/2]).

%% Simulate gpb compiling some proto files,
%% but generate only enough of what's needed for testing -- dummy stuff only.
%% if a bad.proto file is supplied then gpb fails
file(Proto, Opts) ->
    ok = case filename:basename(Proto) of
            "bad.proto" -> error;
            _ -> ok
         end,
    Prefix = proplists:get_value(module_name_prefix, Opts, ""),
    Suffix = proplists:get_value(module_name_suffix, Opts, ""),
    ProtoBase = filename:basename(Proto, ".proto"),
    ModBase = Prefix ++ ProtoBase ++ Suffix,
    ErlDest = filename:join(get_erl_outdir(Opts), ModBase ++ ".erl"),
    HrlDest = filename:join(get_hrl_outdir(Opts), ModBase ++ ".hrl"),
    ok = file:write_file(ErlDest, erl_text(ModBase)),
    ok = file:write_file(HrlDest, hrl_text(ModBase)).

erl_text(ModBase) ->
    io_lib:format(
      lines(["-module(~p).",
             "-export([encode_msg/1]).",
             "-export([decode_msg/2]).",
             "",
             "encode_msg(some_dummy_msg) -> <<1,2,3>>.",
             "",
             "decode_msg(<<1,2,3>>, _) -> some_dummy_msg."]),
      [list_to_atom(ModBase)]).

hrl_text(ModBase) ->
    io_lib:format(
      lines(["-ifndef(~s_hrl).",
             "-define(~s_hrl, true).",
             "",
             "%% some record definitions would normally go here...",
             ""
             "-endif. %% ~s_hrl"]),
      [ModBase, ModBase, ModBase]).

get_erl_outdir(Opts) ->
    proplists:get_value(o_erl, Opts, get_outdir(Opts)).

get_hrl_outdir(Opts) ->
    proplists:get_value(o_hrl, Opts, get_outdir(Opts)).

get_outdir(Opts) ->
    proplists:get_value(o, Opts, ".").

lines(Lines) ->
    lists:flatten([[L, $\n] || L <- Lines]).
