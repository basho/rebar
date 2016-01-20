%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2015 Luis Rascao (luis.rascao@gmail.com)
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
-module(appup_src_2_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {copy, "src", "src"}
    ] ++ inttest_utils:rebar_setup().

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    {ok, [_Pid|Output]} = retest:sh("./rebar compile -vv",
                                    [{async, false}]),

    LineRegexp = "Compiled src/app\.appup\.src",
    ?assertEqual(true, has_line(Output, LineRegexp)),

    %% check that ebin/app.appup exists
    ?assertMatch(true, filelib:is_regular("ebin/app.appup")),
    retest_log:log(debug, "Generated ebin/app.appup~n", []),

    %% check that ebin/app.appup has expected version
    {ok, [{AppVersion, [], []}]} =
        file:consult("ebin/app.appup"),
    ?assertEqual(AppVersion, "1.24"),
    ok.

has_line([], _RE) ->
    false;
has_line([L|T], RE) ->
    case re:run(L, RE, []) of
        {match, _Captured} ->
            true;
        match ->
            true;
        nomatch ->
            has_line(T, RE)
    end.
