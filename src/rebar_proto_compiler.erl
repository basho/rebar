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
-module(rebar_proto_compiler).

-export([compile/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

-record(proto_compiler,
        {key        :: atom(), % Corresponds to the {proto_compiler,Key}
         module     :: atom()
        }).

%% ===================================================================
%% Public API
%% ===================================================================

find_proto_files(ProtoDirs) ->
    lists:foldl(fun(ProtoDir, Acc) ->
                  rebar_utils:find_files_by_ext(ProtoDir, ".proto") ++ Acc
                end,
                [], ProtoDirs).

compile(Config, AppFile) ->
    %% Find a compiler for protocol buffers,
    %% use that for compiling protocol buffers
    {CompilerModule, ProtoDirs} = select_proto_compiler_and_dir(Config),
    case find_proto_files(ProtoDirs) of
        [] ->
            ok;
        Protos ->
            %% Ask the proto compiler to compile the .proto files.
            CompilerModule:proto_compile(Config, AppFile, Protos)
    end.

clean(Config, AppFile) ->
    %% Find a compiler for protocol buffers,
    %% use that for clean protocol buffers
    {CompilerModule, ProtoDirs} = select_proto_compiler_and_dir(Config),
    %% Get a list of generated .beam and .hrl files and then delete them
    case find_proto_files(ProtoDirs) of
        [] ->
            ok;
        Protos ->
            %% Ask the proto compiler to clean the .proto files.
            CompilerModule:proto_clean(Config, AppFile, Protos)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Build protocol buffer (*.proto) sources", compile);
info(help, clean) ->
    info_help("Delete protocol buffer (*.proto) build results", clean).

info_help(GeneralDescr, Cmd) ->
    ?CONSOLE(
       "~s.~n"
       ++ "~n"
       ++ "Valid rebar.config options:~n"
       ++ " {proto_opts, [~n"
       ++ "   {compiler, Compiler},~n"
       ++ "   {src_dirs, [Dir]}~n"
       ++ " ]}~n"
       ++ "The following protocol buffer compilers are available:~n"
       ++ "~s~n",
       [GeneralDescr, format_proto_compiler_list()]),
    %% Print info for each proto compiler
    _ = [begin
             ?CONSOLE("--- ~p ---~n", [Key]),
             CompilerModule:proto_info(help, Cmd)
         end
         || #proto_compiler{key=Key,
                            module=CompilerModule} <- find_proto_compilers()],
    ok.

get_default_compiler() ->
    protobuffs.

find_proto_compilers() ->
    {ok, RebarModuleGroups} = application:get_env(rebar, modules),
    {app_dir, Modules} = lists:keyfind(app_dir, 1, RebarModuleGroups),
    [#proto_compiler{key = M:key(),
                     module = M}
     || M <- Modules,
        is_proto_compiler_module(M)].

is_proto_compiler_module(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            lists:all(fun({Function, Arity}) ->
                              erlang:function_exported(Module, Function, Arity)
                      end,
                      [{key, 0},
                       {proto_compile, 3},
                       {proto_clean, 3},
                       {proto_info, 2}]);
        _ ->
            false
    end.

select_proto_compiler_and_dir(Config) ->
    Default = get_default_compiler(),
    ProtoOpts = rebar_config:get_local(Config, proto_opts, []),
    Key = proplists:get_value(compiler, ProtoOpts, Default),
    ProtoDirs = proplists:get_value(src_dirs, ProtoOpts, ["src"]),
    AvailCompilers = find_proto_compilers(),
    CompilerModule = case lists:keyfind(Key, #proto_compiler.key, AvailCompilers) of
                      #proto_compiler{module=Module} ->
                          Module;
                      false ->
                          ?ABORT("No such protocol buffer compiler known, '~s'~n"
                                 ++ "The following are known:~n"
                                 ++ "~s~n",
                                 [Key, format_proto_compiler_list()])
                     end,
    {CompilerModule, ProtoDirs}.

format_proto_compiler_list() ->
    Default = get_default_compiler(),
    Keys = [Key || #proto_compiler{key=Key} <- find_proto_compilers()],
    Annotations = [if Key == Default -> " (default)";
                      true -> ""
                   end
                   || Key <- Keys],
    lists:flatten(
      [?FMT("  ~p~s~n", [Key, Annotation])
       || {Key, Annotation} <- lists:zip(Keys, Annotations)]).
