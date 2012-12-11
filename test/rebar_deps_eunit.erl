%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2012 Thijs Terlouw (Thijs.Terlouw@spilgames.com)
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
%% @author Thijs Terlouw <thijsterlouw@spilgames.com>
%% @doc This tests functionality in the rebar_deps.
%% -------------------------------------------------------------------
-module(rebar_deps_eunit).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

kv_test_() ->
    [
        ?_assertEqual(default, rebar_deps:get_value(a, default)),
        ?_assertEqual(ok, rebar_deps:set_value(a, b)),
        ?_assertEqual(b, rebar_deps:get_value(a, default)),
        ?_assertEqual(ok, rebar_deps:del_key(a)),  
        ?_assertEqual(default, rebar_deps:get_value(a, default))                                    
    ].

kv_complex_key_test_() ->
    Key1 = {a,1},
    Key2 = {a,2},
    [
        ?_assertEqual(default, rebar_deps:get_value(Key1, default)),
        ?_assertEqual(ok, rebar_deps:set_value(Key1, [{a,1}])),
        ?_assertEqual(ok, rebar_deps:set_value(Key2, [{a,2}])),
        ?_assertEqual([{a,1}], rebar_deps:get_value(Key1, default)),
        ?_assertEqual(ok, rebar_deps:del_key(Key1)), 
        ?_assertEqual([{a,2}], rebar_deps:get_value(Key2, default))
    ].