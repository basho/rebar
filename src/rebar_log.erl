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
-module(rebar_log).

-export([init/1,
         set_level/1,
         error_level/0,
         default_level/0,
         log/3,
         log/4,
         is_verbose/1]).

-define(ERROR_LEVEL, 0).
-define(WARN_LEVEL,  1).
-define(INFO_LEVEL,  2).
-define(DEBUG_LEVEL, 3).

%% ===================================================================
%% Public API
%% ===================================================================

init(Config) ->
    Verbosity = rebar_config:get_global(Config, verbose, default_level()),
    case valid_level(Verbosity) of
        ?ERROR_LEVEL -> set_level(error);
        ?WARN_LEVEL  -> set_level(warn);
        ?INFO_LEVEL  -> set_level(info);
        ?DEBUG_LEVEL -> set_level(debug)
    end,
    LogColored = rebar_config:get_global(Config, log_colored, true),
    set_log_colored(LogColored).


set_level(Level) ->
    erlang:put(rebar_log_level, Level).

set_log_colored(true) ->
    erlang:put(rebar_log_colored, true),
    ok;
set_log_colored(_LogColored) ->
    erlang:put(rebar_log_colored, false),
    ok.

log(Level, Str, Args) ->
    log(standard_io, Level, Str, Args).

log(Device, Level, Str, Args) ->
    LogLevel = erlang:get(rebar_log_level),
    LogColored = erlang:get(rebar_log_colored),
    case should_log(LogLevel, Level) of
        true ->
            io:format(Device, log_prefix(Level, LogColored) ++ Str, Args);
        false ->
            ok
    end.

error_level() -> ?ERROR_LEVEL.
default_level() -> ?WARN_LEVEL.

is_verbose(Config) ->
    rebar_config:get_xconf(Config, is_verbose, false).

%% ===================================================================
%% Internal functions
%% ===================================================================

valid_level(Level) ->
    erlang:max(?ERROR_LEVEL, erlang:min(Level, ?DEBUG_LEVEL)).

should_log(debug, _)     -> true;
should_log(info, debug)  -> false;
should_log(info, _)      -> true;
should_log(warn, debug)  -> false;
should_log(warn, info)   -> false;
should_log(warn, _)      -> true;
should_log(error, error) -> true;
should_log(error, _)     -> false;
should_log(_, _)         -> false.

log_prefix(Level, _Colored = false) ->
    log_prefix(Level);
log_prefix(Level, _Colored = true) ->
    color_from_level(Level) ++ log_prefix(Level) ++ reset_color().

log_prefix(debug) -> "DEBUG: ";
log_prefix(info)  -> "INFO:  ";
log_prefix(warn)  -> "WARN:  ";
log_prefix(error) -> "ERROR: ".

color_from_level(debug) ->
    color_foreground(blue);
color_from_level(info) ->
    color_foreground(green);
color_from_level(warn) ->
    color_foreground(yellow);
color_from_level(error) ->
    color_bold() ++ color_foreground(red).

color_foreground(black)   -> "\e[30m";
color_foreground(red)     -> "\e[31m";
color_foreground(green)   -> "\e[32m";
color_foreground(yellow)  -> "\e[33m";
color_foreground(blue)    -> "\e[34m";
color_foreground(magenta) -> "\e[35m";
color_foreground(cyan)    -> "\e[36m";
color_foreground(white)   -> "\e[37m".

color_bold()  -> "\e[1m".
reset_color() -> "\e[0m".
