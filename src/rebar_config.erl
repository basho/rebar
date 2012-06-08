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
-module(rebar_config).

-export([new/0, new/1, base_config/1, consult_file/1,
         consult_script/1, consult_files/2,
         get/3, get_local/3, get_list/3,
         get_all/2,
         set/3,
         set_global/2, get_global/2,
         is_verbose/0, get_jobs/0,
         set_env/3, get_env/2]).

-include("rebar.hrl").

-record(config, { dir :: file:filename(),
                  opts = [] :: list(),
                  envs = new_env() :: dict() }).

%% Types that can be used from other modules -- alphabetically ordered.
-export_type([config/0]).

%% data types
-opaque config() :: #config{}.

%% ===================================================================
%% Public API
%% ===================================================================

base_config(Parent = #config{opts=Opts0}) ->
    case rebar_config:get_global(config, undefined) of
        undefined ->
            new(Opts0, get_formats(Parent));
        ConfName ->
            %% Someone passed a custom config file,
            %% so we get all formats that ends up
            %% with the same extension.
            new(Opts0, best_format_match(ConfName, Parent))
    end.

new() ->
    #config{dir = rebar_utils:get_cwd()}.

new(ConfigDir) when is_list(ConfigDir) ->
    case consult_files(ConfigDir, default_formats()) of
        {ok, Opts} ->
            #config { dir = rebar_utils:get_cwd(),
                      opts = Opts };
        _ ->
            new()
    end;
new(Parent=#config{opts=Opts0})->
    new(Opts0, get_formats(Parent)).

new(Opts0, Formats) ->
    %% Load terms from rebar.config, if it exists
    Dir  = rebar_utils:get_cwd(),
    Opts = case consult_files(Dir, Formats) of
               {ok, Terms} ->
                   %% Found a config file with some terms. We need to
                   %% be able to distinguish between local definitions
                   %% (i.e. from the file in the cwd) and inherited
                   %% definitions. To accomplish this, we use a marker
                   %% in the proplist (since order matters) between
                   %% the new and old defs.
                   Terms ++ [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               {error, enoent} ->
                   [local] ++
                       [Opt || Opt <- Opts0, Opt /= local]
           end,

    #config{dir = Dir, opts = Opts}.

get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

get_list(Config, Key, Default) ->
    get(Config, Key, Default).

get_local(Config, Key, Default) ->
    proplists:get_value(Key, local_opts(Config#config.opts, []), Default).

get_all(Config, Key) ->
    proplists:get_all_values(Key, Config#config.opts).

set(Config, Key, Value) ->
    Opts = proplists:delete(Key, Config#config.opts),
    Config#config { opts = [{Key, Value} | Opts] }.

set_global(jobs=Key, Value) when is_list(Value) ->
    set_global(Key, list_to_integer(Value));
set_global(jobs=Key, Value) when is_integer(Value) ->
    application:set_env(rebar_global, Key, erlang:max(1, Value));
set_global(Key, Value) ->
    application:set_env(rebar_global, Key, Value).

get_global(Key, Default) ->
    case application:get_env(rebar_global, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

is_verbose() ->
    DefaulLevel = rebar_log:default_level(),
    get_global(verbose, DefaulLevel) > DefaulLevel.

get_jobs() ->
    get_global(jobs, 3).

%% Search the given filenames in the given directory
%% invoking the first one that exists passing the
%% full file path as argument.
%%
%% The invoked callbacks must return { ok, Terms } or
%% { error, Reason }.
consult_files(Dir, [{ File, { Mod, Fun } }|T]) ->
    Full = filename:join([Dir, File]),
    case filelib:is_regular(Full) of
        true  ->
            case apply(Mod, Fun, [Full]) of
                { error, Reason } ->
                    ?ABORT("Failed to load ~s: ~p\n", [Full, Reason]);
                Ok -> Ok
            end;
        false -> consult_files(Dir, T)
    end;

consult_files(_Dir, []) ->
    { error, enoent }.

consult_script(File) ->
    consult_and_eval(remove_script_ext(File), File).

%% consult_file considering if it is a script or not.
%% If so, uses consult_script otherwise fallbacks to
%% file:consult.
%%
%% TODO Consider deprecating it in favor of consult_files.
consult_file(File) ->
    case filename:extension(File) of
        ".script" ->
            consult_script(File);
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    consult_script(Script);
                false ->
                    ?DEBUG("Consult config file ~p~n", [File]),
                    file:consult(File)
            end
    end.

set_env(Config, Mod, Env) ->
    OldEnvs = Config#config.envs,
    NewEnvs = dict:store(Mod, Env, OldEnvs),
    Config#config{envs=NewEnvs}.

get_env(Config, Mod) ->
    dict:fetch(Mod, Config#config.envs).

%% ===================================================================
%% Internal functions
%% ===================================================================

consult_and_eval(File, Script) ->
    ?DEBUG("Evaluating config script ~p~n", [Script]),
    ConfigData = try_consult(File),
    file:script(Script, bs([{'CONFIG', ConfigData}, {'SCRIPT', Script}])).

remove_script_ext(F) ->
    "tpircs." ++ Rev = lists:reverse(F),
    lists:reverse(Rev).

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            ?DEBUG("Consult config file ~p~n", [File]),
            Terms;
        {error, enoent}  -> [];
        {error, Reason} ->
            ?ABORT("Failed to read config file ~s: ~p~n", [File, Reason])
    end.

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

local_opts([], Acc) ->
    lists:reverse(Acc);
local_opts([local | _Rest], Acc) ->
    lists:reverse(Acc);
local_opts([Item | Rest], Acc) ->
    local_opts(Rest, [Item | Acc]).

new_env() ->
    dict:new().

get_formats(Config) ->
    lists:flatten(get_all(Config, rebar_config_formats))
        ++ default_formats().

default_formats() ->
    [
        { "rebar.config.script", { ?MODULE, consult_script } },
        { "rebar.config", { file, consult } }
    ].

best_format_match(Given, Config) ->
    Ext = filename:extension(Given),
    [{ Given, Loc } || { File, Loc } <- get_formats(Config),
        filename:extension(File) == Ext].