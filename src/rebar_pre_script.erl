%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
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
-module(rebar_pre_script).

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _) ->
    execute_pre_script(Config, compile_pre_script).

clean(Config, _) ->
    execute_pre_script(Config, clean_pre_script).


%% ===================================================================
%% Internal functions
%% ===================================================================

execute_pre_script(Config, Key) ->
    case rebar_config:get_local(Config, Key, undefined) of
        undefined ->
            ok;
        Script ->
            deprecated(Key),
            {ok, _} = rebar_utils:sh(Script, []),
            ok
    end.

deprecated(compile_pre_script) ->
     ?CONSOLE(
       <<
         "Config option 'compile_pre_script' has been deprecated in favor"
         " of ~noption {pre_compile, \"script\"}.~nFuture builds of rebar "
         "will remove the option 'compile_pre_script'.~n~n"
       >>, []);
deprecated(clean_pre_script) ->
     ?CONSOLE(
       <<
         "Config option 'clean_pre_script' has been deprecated in favor"
         " of ~noption {pre_clean, \"script\"}.~nFuture builds of rebar "
         "will remove the option 'clean_pre_script'.~n~n"
       >>, []).
