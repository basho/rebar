%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(foo_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_Type, _Args) -> foo_sup:start_link().

stop(_State) -> ok.
