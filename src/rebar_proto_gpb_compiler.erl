%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Tomas Abrahamsson (tomas.abrahamsson@gmail.com)
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
-module(rebar_proto_gpb_compiler).

-export([key/0,
         proto_compile/3,
         proto_clean/3]).

%% for internal use only
-export([proto_info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

key() ->
    gpb.

proto_compile(Config, _AppFile, ProtoFiles) ->
    %% Check for gpb library -- if it's not present, fail
    %% since we have.proto files that need building
    case gpb_is_present() of
        true ->
            UserGpbOpts = user_gpb_opts(Config),
            lists:foreach(
              fun(ProtoFile) ->
                      GpbOpts = UserGpbOpts ++ default_dest_opts()
                          ++ default_include_opts(ProtoFile),

                      case needs_compile(ProtoFile, GpbOpts) of
                          true ->
                              compile_gpb(ProtoFile, GpbOpts);
                          false ->
                              ok
                      end
              end,
              ProtoFiles);
        false ->
            ?ERROR("The gpb library is not present in code path!\n", []),
            ?FAIL
    end.

proto_clean(Config, _AppFile, ProtoFiles) ->
    GpbOpts = user_gpb_opts(Config) ++ default_dest_opts(),
    rebar_file_utils:delete_each(
      [beam_file(F, GpbOpts) || F <- ProtoFiles]
      ++ [erl_file(F, GpbOpts) || F <- ProtoFiles]
      ++ [hrl_file(F, GpbOpts) || F <- ProtoFiles]),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

proto_info(help, compile) ->
    ?CONSOLE(
       "  gpb_opts is passed as options to gpb_compile:file/2.~n"
       "  erl_opts is used when compiling the generated erlang files,~n"
       "    so you might want to add an include path to gpb here,~n"
       "    for gpb.hrl, or else use the include_as_lib gpb_opts option,~n"
       "    or the defs_as_proplists gpb_opts option.~n",
       []);
proto_info(help, clean) ->
    ?CONSOLE("", []).

gpb_is_present() ->
    code:which(gpb) =/= non_existing.

user_gpb_opts(Config) ->
    rebar_config:get_local(Config, gpb_opts, []).

default_dest_opts() ->
    [{o_erl, "src"}, {o_hrl, "include"}].

default_include_opts(Source) ->
    SourceFullPath = filename:absname(Source),
    [{i,filename:dirname(SourceFullPath)}].

needs_compile(ProtoFile, GpbOpts) ->
    Erl = erl_file(ProtoFile, GpbOpts),
    Hrl = hrl_file(ProtoFile, GpbOpts),
    filelib:last_modified(Erl) < filelib:last_modified(ProtoFile) orelse
        filelib:last_modified(Hrl) < filelib:last_modified(ProtoFile).

compile_gpb(Source, GpbOpts) ->
    SourceFullPath = filename:absname(Source),
    ok = filelib:ensure_dir(filename:join("ebin", "dummy")),
    ok = filelib:ensure_dir(filename:join("include", "dummy")),
    ?CONSOLE("Compiling ~s\n", [Source]),
    case gpb_compile:file(SourceFullPath, GpbOpts) of
        ok ->
            ok;
        {error, _Reason} ->
            ?ERROR("Failed to compile ~s~n", [Source]),
            ?FAIL
    end.

beam_file(ProtoFile, GpbOpts) ->
    proto_filename_to_path("ebin", ProtoFile, ".beam", GpbOpts).

erl_file(ProtoFile, GpbOpts) ->
    ErlOutDir = get_erl_outdir(GpbOpts),
    proto_filename_to_path(ErlOutDir, ProtoFile, ".erl", GpbOpts).

hrl_file(ProtoFile, GpbOpts) ->
    HrlOutDir = get_hrl_outdir(GpbOpts),
    proto_filename_to_path(HrlOutDir, ProtoFile, ".hrl", GpbOpts).

proto_filename_to_path(Dir, ProtoFile, NewExt, GpbOpts) ->
    BaseNoExt = filename:basename(ProtoFile, ".proto"),
    Prefix = proplists:get_value(module_name_prefix, GpbOpts, ""),
    Suffix = proplists:get_value(module_name_suffix, GpbOpts, ""),
    filename:join([Dir, Prefix ++ BaseNoExt ++ Suffix ++ NewExt]).

get_erl_outdir(Opts) ->
    proplists:get_value(o_erl, Opts, get_outdir(Opts)).

get_hrl_outdir(Opts) ->
    proplists:get_value(o_hrl, Opts, get_outdir(Opts)).

get_outdir(Opts) ->
    proplists:get_value(o, Opts, ".").
