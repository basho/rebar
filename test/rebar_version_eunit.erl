%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Thijs Terlouw (thijsterlouw@spilgames.com)
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
%% @doc This tests functionality in the rebar_version.
%% -------------------------------------------------------------------
-module(rebar_version_eunit).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

compare_test_() ->
    [
        ?_assertEqual(0, rebar_version:compare({1,2,3}, {1,2,3})),
                      
        ?_assertEqual(-1, rebar_version:compare({1,0,0}, {1,0,1})),
        ?_assertEqual(-1, rebar_version:compare({1,0,0}, {1,1,0})),
        ?_assertEqual(-1, rebar_version:compare({1,0,0}, {2,0,0})),
                      
        ?_assertEqual(1, rebar_version:compare({2,0,0}, {1,0,0})),  
        ?_assertEqual(1, rebar_version:compare({2,0,0}, {1,1,0})),       
        ?_assertEqual(1, rebar_version:compare({2,0,0}, {1,0,1}))                                  
    ].

check_single_constraints_test_() ->
    [
        ?_assertEqual(true, rebar_version:check_constraints("1.2.3", [">= 1.2.3"])),
        ?_assertEqual(true, rebar_version:check_constraints("1.2.3", ["> 1.2.2"])),
        ?_assertEqual(true, rebar_version:check_constraints("1.2.3", ["= 1.2.3"])), 
        ?_assertEqual(true, rebar_version:check_constraints("1.2.3", ["<= 1.2.3"])), 
        ?_assertEqual(true, rebar_version:check_constraints("1.2.3", ["< 1.2.4"]))                                 
    ].

check_multiple_constraints_test_() ->
    [
        ?_assertEqual(true, rebar_version:check_constraints("1.2.3", [">= 1.0.0", "<= 2.0.0"]))                                
    ].