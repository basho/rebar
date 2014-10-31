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

proto_compile(Config, _AppFile, _ProtoFiles) ->
    %% Check for gpb library -- if it's not present, fail
    %% since we have.proto files that need building
    case gpb_is_present() of
        true ->
            rebar_base_compiler:run(Config, [],
                                    "src", ".proto",
                                    "src", ".erl",
                                    fun compile_gpb/3,
                                    [{check_last_mod, true}]);
        false ->
            ?ERROR("The gpb library is not present in code path!\n", []),
            ?FAIL
    end.

proto_clean(Config, _AppFile, ProtoFiles) ->
    GpbOpts = gpb_opts(Config),
    MPrefix = proplists:get_value(module_name_prefix, GpbOpts, ""),
    MSuffix = proplists:get_value(module_name_suffix, GpbOpts, ""),
    rebar_file_utils:delete_each(
      [beam_relpath(MPrefix, F, MSuffix) || F <- ProtoFiles]
      ++ [erl_relpath(MPrefix, F, MSuffix) || F <- ProtoFiles]
      ++ [hrl_relpath(MPrefix, F, MSuffix) || F <- ProtoFiles]),
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

gpb_opts(Config) ->
    rebar_config:get_local(Config, gpb_opts, []).

gpb_is_present() ->
    code:which(gpb) =/= non_existing.

compile_gpb(Source, _Target, Config) ->
    SourceFullPath = filename:absname(Source),
    DefaultDestOpts = [{o_erl, "src"}, {o_hrl, "include"}],
    SelfIncludeOpt = [{i,filename:dirname(SourceFullPath)}],
    GpbOpts = gpb_opts(Config) ++ DefaultDestOpts ++ SelfIncludeOpt,
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

beam_relpath(Prefix, Proto, Suffix) ->
    proto_filename_to_relpath("ebin", Prefix, Proto, Suffix, ".beam").

erl_relpath(Prefix, Proto, Suffix) ->
    proto_filename_to_relpath("src", Prefix, Proto, Suffix, ".erl").

hrl_relpath(Prefix, Proto, Suffix) ->
    proto_filename_to_relpath("include", Prefix, Proto, Suffix, ".hrl").

proto_filename_to_relpath(Dir, Prefix, Proto, Suffix, NewExt) ->
    BaseNoExt = filename:basename(Proto, ".proto"),
    filename:join([Dir, Prefix ++ BaseNoExt ++ Suffix ++ NewExt]).

