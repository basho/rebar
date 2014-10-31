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
-module(rebar_protobuffs_compiler).

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
    protobuffs.

proto_compile(Config, _AppFile, ProtoFiles) ->
    %% Check for protobuffs library -- if it's not present, fail
    %% since we have.proto files that need building
    case protobuffs_is_present() of
        true ->
            %% Build a list of output files - { Proto, Beam, Hrl }
            Targets = [{Proto, beam_file(Proto), hrl_file(Proto)} ||
                          Proto <- ProtoFiles],

            %% Compile each proto file
            compile_each(Config, Targets);
        false ->
            ?ERROR("Protobuffs library not present in code path!\n",
                   []),
            ?FAIL
    end.

proto_clean(_Config, _AppFile, ProtoFiles) ->
    %% Get a list of generated .beam and .hrl files and then delete them
    BeamFiles = [fq_beam_file(F) || F <- ProtoFiles],
    HrlFiles = [fq_hrl_file(F) || F <- ProtoFiles],
    Targets = BeamFiles ++ HrlFiles,
    delete_each(Targets).

%% ===================================================================
%% Internal functions
%% ===================================================================

proto_info(help, compile) ->
    info_help();
proto_info(help, clean) ->
    info_help().

info_help() ->
    ?CONSOLE(
       "Valid rebar.config options:~n"
       "  erl_opts is passed as compile_flags to "
       "protobuffs_compile:scan_file/2~n",
       []).

protobuffs_is_present() ->
    code:which(protobuffs_compile) =/= non_existing.

beam_file(Proto) ->
    filename:basename(Proto, ".proto") ++ "_pb.beam".

hrl_file(Proto) ->
    filename:basename(Proto, ".proto") ++ "_pb.hrl".

fq_beam_file(Proto) ->
    filename:join(["ebin", filename:basename(Proto, ".proto") ++ "_pb.beam"]).

fq_hrl_file(Proto) ->
    filename:join(["include", filename:basename(Proto, ".proto") ++ "_pb.hrl"]).

needs_compile(Proto, Beam) ->
    ActualBeam = filename:join(["ebin", filename:basename(Beam)]),
    filelib:last_modified(ActualBeam) < filelib:last_modified(Proto).

compile_each(_, []) ->
    ok;
compile_each(Config, [{Proto, Beam, Hrl} | Rest]) ->
    case needs_compile(Proto, Beam) of
        true ->
            ?CONSOLE("Compiling ~s\n", [Proto]),
            ErlOpts = rebar_utils:erl_opts(Config),
            case protobuffs_compile:scan_file(Proto,
                                              [{compile_flags,ErlOpts}]) of
                ok ->
                    %% Compilation worked, but we need to move the
                    %% beam and .hrl file into the ebin/ and include/
                    %% directories respectively
                    %% TODO: Protobuffs really needs to be better about this
                    ok = filelib:ensure_dir(filename:join("ebin","dummy")),
                    ok = rebar_file_utils:mv(Beam, "ebin"),
                    ok = filelib:ensure_dir(filename:join("include", Hrl)),
                    ok = rebar_file_utils:mv(Hrl, "include"),
                    ok;
                Other ->
                    ?ERROR("Protobuffs compile of ~s failed: ~p\n",
                           [Proto, Other]),
                    ?FAIL
            end;
        false ->
            ok
    end,
    compile_each(Config, Rest).

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
