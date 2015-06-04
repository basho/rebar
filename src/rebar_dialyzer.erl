%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014-2015 Tuncer Ayaz
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
-module(rebar_dialyzer).

-export([
         dialyze/2,
         'build-plt'/2,
         'check-plt'/2,
         'delete-plt'/2
        ]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

dialyze(Config, AppFile) ->
    Opts = opts(Config),
    {NewConfig, Plt} = plt(Config, AppFile, Opts),
    ok = check_plt_existence(Plt),

    Args = [
            {analysis_type, succ_typings},
            %% http://erlang.org/pipermail/erlang-bugs/2015-February/004781.html
            %% TODO: remove once the minimum required Erlang/OTP release
            %% includes a Dialyzer version without the bug, or alternatively
            %% add a config option to always check the PLT, as this may be
            %% needed by some users.
            {check_plt, false},
            {init_plt, Plt},
            {files_rec, ["ebin"]},
            {warnings, warnings(Opts)}
           ],
    ?DEBUG("dialyze opts:~n~p~n", [Args]),
    case run(Args) of
        [] ->
            {ok, NewConfig};
        Ws ->
            print_warnings(Ws, fullpath),
            ?FAIL
    end.

'build-plt'(Config, AppFile) ->
    Opts = opts(Config),
    {Config1, AppDirs} = app_dirs(Config, AppFile, Opts),
    {NewConfig, Plt} = plt(Config1, AppFile, Opts),

    Args = [
            {analysis_type, plt_build},
            {output_plt, Plt},
            {files_rec, AppDirs}
           ],
    ?DEBUG("build-plt opts:~n~p~n", [Args]),
    case run(Args) of
        [] ->
            {ok, NewConfig};
        Ws ->
            %% As plt_build may raise warnings but still successfully
            %% create the PLT, we cannot interpret this as failure,
            %% and therefore all we can do is report warnings.
            print_warnings(Ws, basename)
    end.

'check-plt'(Config, AppFile) ->
    Opts = opts(Config),
    {NewConfig, Plt} = plt(Config, AppFile, Opts),
    ok = check_plt_existence(Plt),

    Args = [
            {analysis_type, plt_check},
            %% http://erlang.org/pipermail/erlang-bugs/2015-February/004781.html
            %% Without this, the PLT will be checked twice.
            %% TODO: remove once the minimum required Erlang/OTP release
            %% includes a Dialyzer version without the bug.
            {check_plt, false},
            {init_plt, Plt}
           ],
    ?DEBUG("build-plt opts:~n~p~n", [Args]),
    case run(Args) of
        [] ->
            {ok, NewConfig};
        Ws ->
            print_warnings(Ws, basename),
            ?FAIL
    end.

'delete-plt'(Config, AppFile) ->
    Opts = opts(Config),
    {NewConfig, Plt} = plt(Config, AppFile, Opts),
    ?DEBUG("Delete PLT '~s'~n", [Plt]),
    ok = rebar_file_utils:delete_each([Plt]),
    {ok, NewConfig}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, dialyze) ->
    info_help("Analyze the code for discrepancies");
info(help, 'build-plt') ->
    info_help("Build project-specific PLT");
info(help, 'check-plt') ->
    info_help("Check the PLT for consistency and rebuild it if it"
              " is not up-to-date");
info(help, 'delete-plt') ->
    info_help("Delete project-specific PLT").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        Description,
        {dialyzer,
         [
          {plt_location, local},
          {plt_location, "custom_dir"},
          {plt_extra_apps, [app1, app2]},
          {warnings, [unmatched_returns, error_handling]}
         ]}
       ]).

opts(Config) ->
    rebar_config:get_local(Config, dialyzer, []).

plt(Config, AppFile, Opts) ->
    PltDir = plt_dir(Config, Opts),
    {NewConfig, RawAppName} = rebar_app_utils:app_name(Config, AppFile),
    AppName = atom_to_list(RawAppName),
    OtpRel = rebar_utils:otp_release(),
    Plt = filename:join([PltDir, AppName ++ "_" ++ OtpRel ++ "_plt"]),
    ok = filelib:ensure_dir(Plt),
    {NewConfig, Plt}.

plt_dir(Config, Opts) ->
    Location = proplists:get_value(plt_location, Opts, local),
    plt_dir1(Config, Location).

plt_dir1(_Config, Location) when is_list(Location) ->
    case filelib:is_dir(Location) of
        false ->
            ?ABORT("PLT directory does not exist: ~s~n", [Location]);
        true ->
            Location
    end;
plt_dir1(Config, local) ->
    BaseDir = rebar_utils:base_dir(Config),
    filename:join([BaseDir, ".rebar"]).

check_plt_existence(Plt) ->
    case filelib:is_regular(Plt) of
        true ->
            ok;
        false ->
            ?ABORT("PLT '~s' does not exist.~n"
                   "Please run 'rebar build-plt' first.~n", [Plt])
    end.

%% dialyzer:run/1 wrapper to gracefully fail in case of Dialyzer errors
run(Opts) ->
    try dialyzer:run(Opts) of
        Ws -> Ws
    catch
        throw:{dialyzer_error, Reason} ->
            ?ABORT("Dialyzer error:~n~s~n", [Reason])
    end.

warnings(Opts) ->
    proplists:get_value(warnings, Opts, []).

print_warnings(Ws, Option) ->
    lists:foreach(
      fun(W) ->
              ?CONSOLE("~s~n", [format_warning(W, Option)])
      end,
      Ws).

format_warning(W, Option) ->
    case dialyzer:format_warning(W, Option) of
        ":0: " ++ Unknown ->
            strip_newline(Unknown);
        Warning ->
            strip_newline(Warning)
    end.

%% Warning may or may not have trailing \n.
strip_newline(Warning) ->
    string:strip(Warning, right, $\n).

app_dirs(Config, AppFile, Opts) ->
    {NewConfig, AppFileApps} = app_file_apps(Config, AppFile),
    ?DEBUG("app file apps:~n~p~n", [AppFileApps]),
    Deps = deps_apps(Config),
    ?DEBUG("deps apps:~n~p~n", [Deps]),
    ExtraApps = proplists:get_value(plt_extra_apps, Opts, []),
    ?DEBUG("extra apps:~n~p~n", [ExtraApps]),
    %% erts is assumed, and has to be present unconditionally.
    Erts = [erts],
    Apps = ordsets:from_list(Erts ++ AppFileApps ++ Deps ++ ExtraApps),
    AppDirs = [app_lib_dir(App) || App <- ordsets:to_list(Apps)],
    ?DEBUG("app dirs:~n~p~n", [AppDirs]),
    {NewConfig, AppDirs}.

app_file_apps(Config, AppFile) ->
    rebar_app_utils:app_applications(Config, AppFile).

deps_apps(Config) ->
    [element(1, Dep) || Dep <- rebar_config:get_local(Config, deps, [])].

app_lib_dir(App) ->
    case code:lib_dir(App, ebin) of
        {error, _}=Err ->
            ?ABORT("Failed to get ebin dir for app: ~p~n~p~n", [App, Err]);
        Dir ->
            Dir
    end.
