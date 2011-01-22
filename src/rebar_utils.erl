%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_utils).

-export([get_cwd/0,
         is_arch/1,
         get_arch/0,
         get_temp_dir/0,
         get_temp_filename/0,
         remove_dir/1,
         is_outside_base_dir/1,
         sh/2,
         find_files/2,
         now_str/0,
         ensure_dir/1,
         beam_to_mod/2, beams/1,
         erl_to_mod/1,
         abort/2,
         escript_foldl/3,
         find_executable/1]).

-include("rebar.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.


is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_arch() ->
    Words = integer_to_list(8 * erlang:system_info(wordsize)),
    erlang:system_info(otp_release) ++ "-"
        ++ erlang:system_info(system_architecture) ++ "-" ++ Words.

get_temp_dir() ->
    case os:getenv("TEMP") of
      false ->
          "/tmp";
      Val -> 
          Val
    end.

get_temp_filename() ->
    filename:join(get_temp_dir(), 
        io_lib:format("~p", [erlang:phash2(make_ref())])).

remove_dir(Dir) ->    
    case os:type() of
        {win32, _} ->
            os:cmd("rmdir /S /Q " ++ filename:nativename(Dir));
        _ ->
            os:cmd("rm -rf " ++ Dir ++ " 2>/dev/null")
    end.

is_outside_base_dir(Path) ->
    BaseDir = rebar_config:get_global(base_dir, []),
    not(lists:prefix(BaseDir, Path)).

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->
    ?INFO("sh: ~s\n~p\n", [Command0, Options0]),

    DefaultOptions = [use_stdout, abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    Command = patch_on_windows(Command0),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = open_port({spawn, Command}, PortSettings),

    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, Rc} ->
            ErrorHandler(Command, Rc)
    end.

%% We need a bash shell to execute on windows
%% also the port doesn't seem to close from time to time (mingw)
patch_on_windows(Cmd) ->
    case os:type() of
        {win32,nt} ->
            case find_executable("bash") of
                false -> Cmd;
                Bash ->
                    Bash ++ " -c \"" ++ Cmd ++ "; echo _port_cmd_status_ $?\" "
            end;
        _ ->
            Cmd
    end.

find_files(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F | Acc] end, []).

now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b",
				[Year, Month, Day, Hour, Minute, Second])).

%% TODO: filelib:ensure_dir/1 corrected in R13B04. Remove when we drop
%% support for OTP releases older than R13B04.
ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
        ok ->
            ok;
        {error,eexist} ->
            ok;
        Error ->
            Error
    end.

-spec abort(string(), [term()]) -> no_return().
abort(String, Args) ->
    ?ERROR(String, Args),
    halt(1).

%% TODO: Rename emulate_escript_foldl to escript_foldl and remove
%% this function when the time is right. escript:foldl/3 was an
%% undocumented exported fun and has been removed in R14.
escript_foldl(Fun, Acc, File) ->
    {module, zip} = code:ensure_loaded(zip),
    case erlang:function_exported(zip, foldl, 3) of
        true ->
            emulate_escript_foldl(Fun, Acc, File);
        false ->
            escript:foldl(Fun, Acc, File)
    end.

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            "\"" ++ filename:nativename(Path) ++ "\""
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

expand_sh_flag(return_on_error) ->
    {error_handler,
     fun(_Command, Rc) ->
             {error, Rc}
     end};
expand_sh_flag({abort_on_error, Message}) ->
    {error_handler,
     fun(_Command, _Rc) ->
             ?ABORT(Message, [])
     end};
expand_sh_flag(abort_on_error) ->
    {error_handler,
     fun log_and_abort/2};
expand_sh_flag(use_stdout) ->
    {output_handler,
     fun(Line, Acc) ->
             ?CONSOLE("~s", [Line]),
             [Acc | Line]
     end};
expand_sh_flag({use_stdout, false}) ->
    {output_handler,
     fun(Line, Acc) ->
             [Acc | Line]
     end};
expand_sh_flag({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
    {port_settings, Env}.

-spec log_and_abort(string(), integer()) -> no_return().
log_and_abort(Command, Rc) ->
    ?ABORT("~s failed with error: ~w\n", [Command, Rc]).

sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {_, "_port_cmd_status_ " ++ Status}}} ->
            (catch erlang:port_close(Port)), % sigh () for indentation
            case list_to_integer(Status) of
                0  -> {ok, lists:flatten(Acc)};
                Rc -> {error, Rc}
            end;
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(Acc)};
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.

beam_to_mod(Dir, Filename) ->
    [Dir | Rest] = filename:split(Filename),
    list_to_atom(filename:basename(string:join(Rest, "."), ".beam")).

erl_to_mod(Filename) ->
    list_to_atom(filename:rootname(filename:basename(Filename))).

beams(Dir) ->
    filelib:fold_files(Dir, ".*\.beam\$", true,
                       fun(F, Acc) -> [F | Acc] end, []).

emulate_escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
        {ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
            case Body of
                {source, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {beam, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {archive, ArchiveBin} ->
                    zip:foldl(Fun, Acc, {File, ArchiveBin})
            end;
        {error, _} = Error ->
            Error
    end.
