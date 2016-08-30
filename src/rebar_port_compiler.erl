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
-module(rebar_port_compiler).

-export([compile/2,
         clean/2]).

%% for internal use only
-export([setup_env/1,
         info/2]).

-include("rebar.hrl").

-record(spec, {type::'drv' | 'exe',
               link_lang::'cc' | 'cxx',
               target::file:filename(),
               sources = [] :: [file:filename(), ...],
               objects = [] :: [file:filename(), ...],
               opts = [] :: list() | []}).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, AppFile) ->
    case get_specs(Config, AppFile) of
        [] ->
            ok;
        Specs ->
            SharedEnv = rebar_config:get_env(Config, rebar_deps) ++
                rebar_config:get_env(Config, ?MODULE),

            %% Compile each of the sources
            NewBins = compile_sources(Config, Specs, SharedEnv),

            %% Make sure that the target directories exist
            ?INFO("Using specs ~p\n", [Specs]),
            lists:foreach(fun(#spec{target=Target}) ->
                                  ok = filelib:ensure_dir(Target)
                          end, Specs),

            %% Only relink if necessary, given the Target
            %% and list of new binaries
            lists:foreach(
              fun(#spec{target=Target, objects=Bins, opts=Opts,
                        link_lang=LinkLang}) ->
                      AllBins = [sets:from_list(Bins),
                                 sets:from_list(NewBins)],
                      Intersection = sets:intersection(AllBins),
                      case needs_link(Target, sets:to_list(Intersection)) of
                          true ->
                              LinkTemplate = select_link_template(LinkLang,
                                                                  Target),
                              Env = proplists:get_value(env, Opts, SharedEnv),
                              Cmd = expand_command(LinkTemplate, Env,
                                                   string:join(Bins, " "),
                                                   Target),
                              rebar_utils:sh(Cmd, [{env, Env}]);
                          false ->
                              ?INFO("Skipping relink of ~s\n", [Target]),
                              ok
                      end
              end, Specs)
    end.

clean(Config, AppFile) ->
    case get_specs(Config, AppFile) of
        [] ->
            ok;
        Specs ->
            lists:foreach(fun(#spec{target=Target, objects=Objects}) ->
                                  rebar_file_utils:delete_each([Target]),
                                  rebar_file_utils:delete_each(Objects),
                                  rebar_file_utils:delete_each(port_deps(Objects))
                          end, Specs)
    end,
    ok.

setup_env(Config) ->
    setup_env(Config, []).

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Build port sources");
info(help, clean) ->
    info_help("Delete port build results").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "port_specs - Erlang list of tuples of the forms~n"
       "             {ArchRegex, TargetFile, Sources, Options}~n"
       "             {ArchRegex, TargetFile, Sources}~n"
       "             {TargetFile, Sources}~n"
       "~n"
       "             Examples:~n"
       "             ~p~n"
       "~n"
       "port_env - Erlang list of key/value pairs which will control~n"
       "           the environment when running the compiler and linker.~n"
       "           Variables set in the surrounding system shell are taken~n"
       "           into consideration when expanding port_env.~n"
       "~n"
       "           By default, the following variables are defined:~n"
       "           CC       - C compiler~n"
       "           CXX      - C++ compiler~n"
       "           CFLAGS   - C compiler~n"
       "           CXXFLAGS - C++ compiler~n"
       "           LDFLAGS  - Link flags~n"
       "           ERL_CFLAGS  - default -I paths for erts and ei~n"
       "           ERL_LDFLAGS - default -L and -lerl_interface -lei~n"
       "           DRV_CFLAGS  - flags that will be used for compiling~n"
       "           DRV_LDFLAGS - flags that will be used for linking~n"
       "           EXE_CFLAGS  - flags that will be used for compiling~n"
       "           EXE_LDFLAGS - flags that will be used for linking~n"
       "           ERL_EI_LIBDIR - ei library directory~n"
       "           DRV_CXX_TEMPLATE      - C++ command template~n"
       "           DRV_CC_TEMPLATE       - C command template~n"
       "           DRV_LINK_TEMPLATE     - C Linker command template~n"
       "           DRV_LINK_CXX_TEMPLATE - C++ Linker command template~n"
       "           EXE_CXX_TEMPLATE      - C++ command template~n"
       "           EXE_CC_TEMPLATE       - C command template~n"
       "           EXE_LINK_TEMPLATE     - C Linker command template~n"
       "           EXE_LINK_CXX_TEMPLATE - C++ Linker command template~n"
       "~n"
       "           Note that if you wish to extend (vs. replace) these variables,~n"
       "           you MUST include a shell-style reference in your definition.~n"
       "           e.g. to extend CFLAGS, do something like:~n"
       "~n"
       "           {port_env, [{\"CFLAGS\", \"$CFLAGS -MyOtherOptions\"}]}~n"
       "~n"
       "           It is also possible to specify platform specific options~n"
       "           by specifying a triplet where the first string is a regex~n"
       "           that is checked against Erlang's system architecture string.~n"
       "           e.g. to specify a CFLAG that only applies to x86_64 on linux~n"
       "           do:~n"
       "           {port_env, [{\"x86_64.*-linux\", \"CFLAGS\",~n"
       "                        \"$CFLAGS -X86Options\"}]}~n"
       "~n"
       "Cross-arch environment variables to configure toolchain:~n"
       "  REBAR_TARGET_ARCH to set the tool chain name to use~n"
       "  REBAR_TARGET_ARCH_WORDSIZE (optional - "
       "if CC fails to determine word size)~n"
       "  fallback word size is 32~n"
       "  REBAR_TARGET_ARCH_VSN (optional - "
       "if a special version of CC/CXX is requested)~n",
       [
        Description,
        {port_specs, [{"priv/so_name.so", ["c_src/*.c"]},
                      {"linux", "priv/hello_linux", ["c_src/hello_linux.c"]},
                      {"linux", "priv/hello_linux", ["c_src/*.c"], [{env, []}]}]}
       ]).

setup_env(Config, ExtraEnv) ->
    %% Extract environment values from the config (if specified) and
    %% merge with the default for this operating system. This enables
    %% max flexibility for users.
    DefaultEnv  = filter_env(default_env(), []),

    %% Get any port-specific envs; use port_env first and then fallback
    %% to port_envs for compatibility
    RawPortEnv = rebar_config:get_list(
                   Config,
                   port_env,
                   rebar_config:get_list(Config, port_envs, [])),

    PortEnv = filter_env(RawPortEnv, []),
    Defines = get_defines(Config),
    OverrideEnv = Defines ++ PortEnv ++ filter_env(ExtraEnv, []),
    RawEnv = apply_defaults(os_env(), DefaultEnv) ++ OverrideEnv,
    expand_vars_loop(merge_each_var(RawEnv, [])).

get_defines(Config) ->
    RawDefines = rebar_config:get_xconf(Config, defines, []),
    Defines = string:join(["-D" ++ D || D <- RawDefines], " "),
    [{"ERL_CFLAGS", "$ERL_CFLAGS " ++ Defines}].

replace_extension(File, NewExt) ->
    OldExt = filename:extension(File),
    replace_extension(File, OldExt, NewExt).

replace_extension(File, OldExt, NewExt) ->
    filename:rootname(File, OldExt) ++ NewExt.

%%
%% == compile and link ==
%%

compile_sources(Config, Specs, SharedEnv) ->
    {NewBins, Db} =
        lists:foldl(
          fun(#spec{sources=Sources, type=Type, opts=Opts}, Acc) ->
                  Env = proplists:get_value(env, Opts, SharedEnv),
                  compile_each(Config, Sources, Type, Env, Acc)
          end, {[], []}, Specs),
    %% Rewrite clang compile commands database file only if something
    %% was compiled.
    case NewBins of
        [] ->
            ok;
        _ ->
            {ok, ClangDbFile} = file:open("compile_commands.json", [write]),
            ok = io:fwrite(ClangDbFile, "[~n", []),
            lists:foreach(fun(E) -> ok = io:fwrite(ClangDbFile, E, []) end, Db),
            ok = io:fwrite(ClangDbFile, "]~n", []),
            ok = file:close(ClangDbFile)
    end,
    NewBins.

compile_each(_Config, [], _Type, _Env, {NewBins, CDB}) ->
    {lists:reverse(NewBins), lists:reverse(CDB)};
compile_each(Config, [Source | Rest], Type, Env, {NewBins, CDB}) ->
    Ext = filename:extension(Source),
    Bin = replace_extension(Source, Ext, ".o"),
    Template = select_compile_template(Type, compiler(Ext)),
    Cmd = expand_command(Template, Env, Source, Bin),
    CDBEnt = cdb_entry(Source, Cmd, Rest),
    NewCDB = [CDBEnt | CDB],
    case needs_compile(Source, Bin) of
        true ->
            ShOpts = [{env, Env}, return_on_error, {use_stdout, false}],
            exec_compiler(Config, Source, Cmd, ShOpts),
            compile_each(Config, Rest, Type, Env,
                         {[Bin | NewBins], NewCDB});
        false ->
            ?INFO("Skipping ~s\n", [Source]),
            compile_each(Config, Rest, Type, Env, {NewBins, NewCDB})
    end.

%% Generate a clang compilation db entry for Src and Cmd
cdb_entry(Src, Cmd, SrcRest) ->
    %% Omit all variables from cmd, and use that as cmd in
    %% CDB, because otherwise clang-* will complain about it.
    CDBCmd = string:join(
               lists:filter(
                 fun("$"++_) -> false;
                    (_)      -> true
                 end,
                 string:tokens(Cmd, " ")),
               " "),

    Cwd = rebar_utils:get_cwd(),
    %% If there are more source files, make sure we end the CDB entry
    %% with a comma.
    Sep = case SrcRest of
              [] -> "~n";
              _  -> ",~n"
          end,
    %% CDB entry
    ?FMT("{ \"file\"      : ~p~n"
         ", \"directory\" : ~p~n"
         ", \"command\"   : ~p~n"
         "}~s",
         [Src, Cwd, CDBCmd, Sep]).

exec_compiler(Config, Source, Cmd, ShOpts) ->
    case rebar_utils:sh(Cmd, ShOpts) of
        {error, {_RC, RawError}} ->
            AbsSource = case rebar_utils:processing_base_dir(Config) of
                            true ->
                                Source;
                            false ->
                                filename:absname(Source)
                        end,
            ?CONSOLE("Compiling ~s\n", [AbsSource]),
            Error = re:replace(RawError, Source, AbsSource,
                               [{return, list}, global]),
            ?CONSOLE("~s", [Error]),
            ?FAIL;
        {ok, Output} ->
            ?CONSOLE("Compiling ~s\n", [Source]),
            ?CONSOLE("~s", [Output])
    end.

needs_compile(Source, Bin) ->
    needs_link(Bin, [Source|bin_deps(Bin)]).

%% NOTE: This relies on -MMD being passed to the compiler and returns an
%% empty list if the .d file is not available.  This means header deps are
%% ignored on win32.
bin_deps(Bin) ->
    [DepFile] = port_deps([Bin]),
    case file:read_file(DepFile) of
        {ok, Deps} ->
            Ds = parse_bin_deps(list_to_binary(Bin), Deps),
            ?DEBUG("Deps of ~p: ~p\n", [Bin, Ds]),
            Ds;
        {error, Err} ->
            ?DEBUG("Skipping deps parse of ~s: ~p\n", [DepFile, Err]),
            []
    end.

parse_bin_deps(Bin, Deps) ->
    Sz = size(Bin),
    <<Bin:Sz/binary, ": ", X/binary>> = Deps,
    Ds = re:split(X, "\\s*\\\\\\R\\s*|\\s+", [{return, binary}]),
    [D || D <- Ds, D =/= <<>>].

needs_link(SoName, []) ->
    filelib:last_modified(SoName) == 0;
needs_link(SoName, NewBins) ->
    MaxLastMod = lists:max([filelib:last_modified(B) || B <- NewBins]),
    case filelib:last_modified(SoName) of
        0 ->
            ?DEBUG("Last mod is 0 on ~s\n", [SoName]),
            true;
        Other ->
            ?DEBUG("Checking ~p >= ~p\n", [MaxLastMod, Other]),
            MaxLastMod >= Other
    end.

%%
%% == port_specs ==
%%

get_specs(Config, AppFile) ->
    Specs = case rebar_config:get_local(Config, port_specs, []) of
                [] ->
                    %% No spec provided. Construct a spec
                    %% from old-school so_name and sources
                    [port_spec_from_legacy(Config, AppFile)];
                PortSpecs ->
                    Filtered = filter_port_specs(PortSpecs),
                    OsType = os:type(),
                    [get_port_spec(Config, OsType, Spec) || Spec <- Filtered]
            end,
    [S || S <- Specs, S#spec.sources /= []].

port_spec_from_legacy(Config, AppFile) ->
    %% Get the target from the so_name variable
    Target = case rebar_config:get(Config, so_name, undefined) of
                 undefined ->
                     %% Generate a sensible default from app file
                     {_, AppName} = rebar_app_utils:app_name(Config, AppFile),
                     filename:join("priv",
                                   lists:concat([AppName, "_drv.so"]));
                 AName ->
                     %% Old form is available -- use it
                     filename:join("priv", AName)
             end,
    %% Get the list of source files from port_sources
    Sources = port_sources(rebar_config:get_list(Config, port_sources,
                                                 ["c_src/*.c"])),
    #spec { type = target_type(Target),
            link_lang = cc,
            target = maybe_switch_extension(os:type(), Target),
            sources = Sources,
            objects = port_objects(Sources) }.

filter_port_specs(Specs) ->
    [S || S <- Specs, filter_port_spec(S)].

filter_port_spec({ArchRegex, _, _, _}) ->
    rebar_utils:is_arch(ArchRegex);
filter_port_spec({ArchRegex, _, _}) ->
    rebar_utils:is_arch(ArchRegex);
filter_port_spec({_, _}) ->
    true.

get_port_spec(Config, OsType, {Target, Sources}) ->
    get_port_spec(Config, OsType, {undefined, Target, Sources, []});
get_port_spec(Config, OsType, {Arch, Target, Sources}) ->
    get_port_spec(Config, OsType, {Arch, Target, Sources, []});
get_port_spec(Config, OsType, {_Arch, Target, Sources, Opts}) ->
    SourceFiles = port_sources(Sources),
    LinkLang =
        case lists:any(
               fun(Src) -> compiler(filename:extension(Src)) == "$CXX" end,
               SourceFiles)
        of
            true  -> cxx;
            false -> cc
        end,
    ObjectFiles = port_objects(SourceFiles),
    #spec{type=target_type(Target),
          target=maybe_switch_extension(OsType, Target),
          link_lang=LinkLang,
          sources=SourceFiles,
          objects=ObjectFiles,
          opts=port_opts(Config, Opts)}.

port_sources(Sources) ->
    lists:flatmap(fun filelib:wildcard/1, Sources).

port_objects(SourceFiles) ->
    [replace_extension(O, ".o") || O <- SourceFiles].

port_deps(SourceFiles) ->
    [replace_extension(O, ".d") || O <- SourceFiles].

port_opts(Config, Opts) ->
    [port_opt(Config, O) || O <- Opts].

port_opt(Config, {env, Env}) ->
    {env, setup_env(Config, Env)};
port_opt(_Config, Opt) ->
    Opt.

maybe_switch_extension({win32, nt}, Target) ->
    switch_to_dll_or_exe(Target);
maybe_switch_extension(_OsType, Target) ->
    Target.

switch_to_dll_or_exe(Target) ->
    case filename:extension(Target) of
        ".so"  -> filename:rootname(Target, ".so") ++ ".dll";
        []     -> Target ++ ".exe";
        _Other -> Target
    end.

%%
%% == port_env ==
%%

%%
%% Choose a compiler variable, based on a provided extension
%%
compiler(".cc")  -> "$CXX";
compiler(".cp")  -> "$CXX";
compiler(".cxx") -> "$CXX";
compiler(".cpp") -> "$CXX";
compiler(".CPP") -> "$CXX";
compiler(".c++") -> "$CXX";
compiler(".C")   -> "$CXX";
compiler(_)      -> "$CC".

%%
%% Given a list of {Key, Value} variables, and another list of default
%% {Key, Value} variables, return a merged list where the rule is if the
%% default is expandable expand it with the value of the variable list,
%% otherwise just return the value of the variable.
%%
apply_defaults(Vars, Defaults) ->
    dict:to_list(
      dict:merge(fun(Key, VarValue, DefaultValue) ->
                         case is_expandable(DefaultValue) of
                             true ->
                                 rebar_utils:expand_env_variable(DefaultValue,
                                                                 Key,
                                                                 VarValue);
                             false -> VarValue
                         end
                 end,
                 dict:from_list(Vars),
                 dict:from_list(Defaults))).

%%
%% Given a list of {Key, Value} environment variables, where Key may be defined
%% multiple times, walk the list and expand each self-reference so that we
%% end with a list of each variable singly-defined.
%%
merge_each_var([], Vars) ->
    Vars;
merge_each_var([{Key, Value} | Rest], Vars) ->
    Evalue = case orddict:find(Key, Vars) of
                 error ->
                     %% Nothing yet defined for this key/value.
                     %% Expand any self-references as blank.
                     rebar_utils:expand_env_variable(Value, Key, "");
                 {ok, Value0} ->
                     %% Use previous definition in expansion
                     rebar_utils:expand_env_variable(Value, Key, Value0)
             end,
    merge_each_var(Rest, orddict:store(Key, Evalue, Vars)).

%%
%% Give a unique list of {Key, Value} environment variables, expand each one
%% for every other key until no further expansions are possible.
%%
expand_vars_loop(Vars) ->
    expand_vars_loop(Vars, [], dict:from_list(Vars), 10).

expand_vars_loop(_Pending, _Recurse, _Vars, 0) ->
    ?ABORT("Max. expansion reached for ENV vars!\n", []);
expand_vars_loop([], [], Vars, _Count) ->
    lists:keysort(1, dict:to_list(Vars));
expand_vars_loop([], Recurse, Vars, Count) ->
    expand_vars_loop(Recurse, [], Vars, Count-1);
expand_vars_loop([{K, V} | Rest], Recurse, Vars, Count) ->
    %% Identify the variables that need expansion in this value
    ReOpts = [global, {capture, all_but_first, list}, unicode],
    case re:run(V, "\\\${?(\\w+)}?", ReOpts) of
        {match, Matches} ->
            %% Identify the unique variables that need to be expanded
            UniqueMatches = lists:usort([M || [M] <- Matches]),

            %% For each variable, expand it and return the final
            %% value. Note that if we have a bunch of unresolvable
            %% variables, nothing happens and we don't bother
            %% attempting further expansion
            case expand_keys_in_value(UniqueMatches, V, Vars) of
                V ->
                    %% No change after expansion; move along
                    expand_vars_loop(Rest, Recurse, Vars, Count);
                Expanded ->
                    %% Some expansion occurred; move to next k/v but
                    %% revisit this value in the next loop to check
                    %% for further expansion
                    NewVars = dict:store(K, Expanded, Vars),
                    expand_vars_loop(Rest, [{K, Expanded} | Recurse],
                                     NewVars, Count)
            end;

        nomatch ->
            %% No values in this variable need expansion; move along
            expand_vars_loop(Rest, Recurse, Vars, Count)
    end.

expand_keys_in_value([], Value, _Vars) ->
    Value;
expand_keys_in_value([Key | Rest], Value, Vars) ->
    NewValue = case dict:find(Key, Vars) of
                   {ok, KValue} ->
                       rebar_utils:expand_env_variable(Value, Key, KValue);
                   error ->
                       Value
               end,
    expand_keys_in_value(Rest, NewValue, Vars).

expand_command(TmplName, Env, InFiles, OutFile) ->
    Cmd0 = proplists:get_value(TmplName, Env),
    Cmd1 = rebar_utils:expand_env_variable(Cmd0, "PORT_IN_FILES", InFiles),
    rebar_utils:expand_env_variable(Cmd1, "PORT_OUT_FILE", OutFile).

%%
%% Given a string, determine if it is expandable
%%
is_expandable(InStr) ->
    case re:run(InStr,"\\\$",[{capture,none}]) of
        match -> true;
        nomatch -> false
    end.

%%
%% Filter a list of env vars such that only those which match the provided
%% architecture regex (or do not have a regex) are returned.
%%
filter_env([], Acc) ->
    lists:reverse(Acc);
filter_env([{ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_env(Rest, [{Key, Value} | Acc]);
        false ->
            filter_env(Rest, Acc)
    end;
filter_env([{Key, Value} | Rest], Acc) ->
    filter_env(Rest, [{Key, Value} | Acc]).

erts_dir() ->
    lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]).

os_env() ->
    ReOpts = [{return, list}, {parts, 2}, unicode],
    Os = [list_to_tuple(re:split(S, "=", ReOpts)) ||
             S <- lists:filter(fun discard_deps_vars/1, os:getenv())],
    %% Drop variables without a name (win32)
    [T1 || {K, _V} = T1 <- Os, K =/= []].

%%
%% To avoid having multiple repetitions of the same environment variables
%% (ERL_LIBS), avoid exporting any variables that may cause conflict with
%% those exported by the rebar_deps module (ERL_LIBS, REBAR_DEPS_DIR)
%%
discard_deps_vars("ERL_LIBS=" ++ _Value)       -> false;
discard_deps_vars("REBAR_DEPS_DIR=" ++ _Value) -> false;
discard_deps_vars(_Var)                        -> true.

select_compile_template(drv, Compiler) ->
    select_compile_drv_template(Compiler);
select_compile_template(exe, Compiler) ->
    select_compile_exe_template(Compiler).

select_compile_drv_template("$CC")  -> "DRV_CC_TEMPLATE";
select_compile_drv_template("$CXX") -> "DRV_CXX_TEMPLATE".

select_compile_exe_template("$CC")  -> "EXE_CC_TEMPLATE";
select_compile_exe_template("$CXX") -> "EXE_CXX_TEMPLATE".

select_link_template(LinkLang, Target) ->
    case {LinkLang, target_type(Target)} of
        {cc,  drv} -> "DRV_LINK_TEMPLATE";
        {cxx, drv} -> "DRV_LINK_CXX_TEMPLATE";
        {cc,  exe} -> "EXE_LINK_TEMPLATE";
        {cxx, exe} -> "EXE_LINK_CXX_TEMPLATE"
    end.

target_type(Target) -> target_type1(filename:extension(Target)).

target_type1(".so")  -> drv;
target_type1(".dll") -> drv;
target_type1("")     -> exe;
target_type1(".exe") -> exe.

erl_interface_dir(Subdir) ->
    case code:lib_dir(erl_interface, Subdir) of
        {error, bad_name} ->
            throw({error, {erl_interface,Subdir,"code:lib_dir(erl_interface)"
                           "is unable to find the erl_interface library."}});
        Dir -> Dir
    end.

default_env() ->
    Arch = os:getenv("REBAR_TARGET_ARCH"),
    Vsn = os:getenv("REBAR_TARGET_ARCH_VSN"),
    [
     {"CC", get_tool(Arch, Vsn, "gcc", "cc")},
     {"CXX", get_tool(Arch, Vsn, "g++", "c++")},
     {"AR", get_tool(Arch, "ar", "ar")},
     {"AS", get_tool(Arch, "as", "as")},
     {"CPP", get_tool(Arch, Vsn, "cpp", "cpp")},
     {"LD", get_tool(Arch, "ld", "ld")},
     {"RANLIB", get_tool(Arch, Vsn, "ranlib", "ranlib")},
     {"STRIP", get_tool(Arch, "strip", "strip")},
     {"NM", get_tool(Arch, "nm", "nm")},
     {"OBJCOPY", get_tool(Arch, "objcopy", "objcopy")},
     {"OBJDUMP", get_tool(Arch, "objdump", "objdump")},

     {"DRV_CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"DRV_CC_TEMPLATE",
      "$CC -c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"DRV_LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
     {"DRV_LINK_CXX_TEMPLATE",
      "$CXX $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
     {"EXE_CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"EXE_CC_TEMPLATE",
      "$CC -c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"EXE_LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
     {"EXE_LINK_CXX_TEMPLATE",
      "$CXX $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
     {"DRV_CFLAGS" , "-g -Wall -fPIC -MMD $ERL_CFLAGS"},
     {"DRV_LDFLAGS", "-shared $ERL_LDFLAGS"},
     {"EXE_CFLAGS" , "-g -Wall -fPIC -MMD $ERL_CFLAGS"},
     {"EXE_LDFLAGS", "$ERL_LDFLAGS"},

     {"ERL_CFLAGS", lists:concat(
                      [
                       " -I\"", erl_interface_dir(include),
                       "\" -I\"", filename:join(erts_dir(), "include"),
                       "\" "
                      ])},
     {"ERL_EI_LIBDIR", lists:concat(["\"", erl_interface_dir(lib), "\""])},
     {"ERL_LDFLAGS"  , " -L$ERL_EI_LIBDIR -lerl_interface -lei"},
     {"ERLANG_ARCH"  , rebar_utils:wordsize()},
     {"ERLANG_TARGET", rebar_utils:get_arch()},

     {"darwin", "DRV_LDFLAGS",
      "-bundle -flat_namespace -undefined suppress $ERL_LDFLAGS"},

     %% Solaris specific flags
     {"solaris.*-64$", "CFLAGS", "-D_REENTRANT -m64 $CFLAGS"},
     {"solaris.*-64$", "CXXFLAGS", "-D_REENTRANT -m64 $CXXFLAGS"},
     {"solaris.*-64$", "LDFLAGS", "-m64 $LDFLAGS"},

     %% OS X Leopard flags for 64-bit
     {"darwin9.*-64$", "CFLAGS", "-m64 $CFLAGS"},
     {"darwin9.*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
     {"darwin9.*-64$", "LDFLAGS", "-arch x86_64 -flat_namespace -undefined suppress $LDFLAGS"},

     %% OS X Lion onwards flags for 64-bit
     {"darwin1[0-4].*-64$", "CFLAGS", "-m64 $CFLAGS"},
     {"darwin1[0-4].*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
     {"darwin1[0-4].*-64$", "LDFLAGS", "-arch x86_64 -flat_namespace -undefined suppress $LDFLAGS"},

     %% OS X Snow Leopard, Lion, and Mountain Lion flags for 32-bit
     {"darwin1[0-2].*-32", "CFLAGS", "-m32 $CFLAGS"},
     {"darwin1[0-2].*-32", "CXXFLAGS", "-m32 $CXXFLAGS"},
     {"darwin1[0-2].*-32", "LDFLAGS", "-arch i386 -flat_namespace -undefined suppress $LDFLAGS"},

     %% Windows specific flags
     %% add MS Visual C++ support to rebar on Windows
     {"win32", "CC", "cl.exe"},
     {"win32", "CXX", "cl.exe"},
     {"win32", "LINKER", "link.exe"},
     {"win32", "DRV_CXX_TEMPLATE",
      %% DRV_* and EXE_* Templates are identical
      "$CXX /c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "DRV_CC_TEMPLATE",
      "$CC /c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "DRV_LINK_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS /OUT:$PORT_OUT_FILE"},
     %% DRV_* and EXE_* Templates are identical
     {"win32", "EXE_CXX_TEMPLATE",
      "$CXX /c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "EXE_CC_TEMPLATE",
      "$CC /c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "EXE_LINK_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS /OUT:$PORT_OUT_FILE"},
     %% ERL_CFLAGS are ok as -I even though strictly it should be /I
     {"win32", "ERL_LDFLAGS",
      " /LIBPATH:$ERL_EI_LIBDIR erl_interface.lib ei.lib"},
     {"win32", "DRV_CFLAGS", "/Zi /Wall $ERL_CFLAGS"},
     {"win32", "DRV_LDFLAGS", "/DLL $ERL_LDFLAGS"}
    ].

get_tool(Arch, Tool, Default) ->
    get_tool(Arch, false, Tool, Default).

get_tool(false, _, _, Default) -> Default;
get_tool("", _, _, Default) -> Default;
get_tool(Arch, false, Tool, _Default) -> Arch++"-"++Tool;
get_tool(Arch, "", Tool, _Default) -> Arch++"-"++Tool;
get_tool(Arch, Vsn, Tool, _Default) -> Arch++"-"++Tool++"-"++Vsn.
