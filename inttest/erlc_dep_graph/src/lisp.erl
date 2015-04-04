%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et
-module(lisp).

-export([parse_transform/2]).

-include("lambda.hrl").
-ifdef(NOT_DEFINED).
-include_lib("include/non/existent.hrl").
-endif.

parse_transform(Forms, _Options) ->
    Forms.
