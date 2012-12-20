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
%% Partial implementation of Semantic Versioning (see http://semver.org/ )
%% Currently only supports X.Y.Z where X, Y and Z are integers
%% -------------------------------------------------------------------
-module(rebar_version).

-include("rebar.hrl").

-export([check/2]).

%% For testing:
-export([compare/2, check_constraints/2]).

-type version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.       % {Major, Minor, Patch}
-type comparator() :: string().                                                     % ">" | ">=" | "=" | "<=" | "<" .
-type constraint() :: {comparator(), version()}.
-type constraints() :: [constraint()].


%% ====================================================================
%% Public API
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% New detailed version support
%% example: [ ">= 1.0.0", "< 2.0.0" ]
-spec check(string(), [string()] | string()) -> boolean().
%% @end
%%--------------------------------------------------------------------
check(Vsn, [Constraint|_] = VsnConstraints) when not is_integer(Constraint) andalso length(Constraint) > 1 ->
    case check_constraints(Vsn, VsnConstraints) of
        true ->
            true;
        _ ->
            false
    end;
%% Old version support with regex
%% example: "0.5.*"
check(Vsn, VsnRegex) ->
    case re:run(Vsn, VsnRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check if all constraints are satisfied.
-spec check_constraints(string(), [string()]) -> boolean().
%% @end
%%--------------------------------------------------------------------
check_constraints(Vsn, Constraints) ->
    ParsedVersion = parse_version(Vsn),
    ParsedConstraints = parse_constraints(Constraints),
    verify_constraints(ParsedVersion, ParsedConstraints, true).

verify_constraints(_, _, false) ->
    false;
verify_constraints(_, [], Result) ->
    Result;
verify_constraints(Vsn, [{Comparator, ConstraintVsn}|Tail], _) ->
    Comparison = compare(Vsn, ConstraintVsn),
    Result = case Comparator of
                ">" -> Comparison > 0;
                ">=" -> Comparison >= 0;
                "=" -> Comparison =:= 0;
                "<=" -> Comparison =< 0;
                "<" -> Comparison < 0
            end,
    verify_constraints(Vsn, Tail, Result).


%%--------------------------------------------------------------------
%% @doc
%% Parsing each constraint into a comparator and a version.
-spec parse_constraints([string()]) -> constraints().
%% @end
%%--------------------------------------------------------------------
parse_constraints(VsnConstraints) ->
    parse_constraints(VsnConstraints, []).

parse_constraints([], Acc) ->
    Acc;
parse_constraints([H|T], Acc) ->
    parse_constraints(T, [ parse_constraint(H) | Acc]).

parse_constraint(Constraint) ->
    case string:tokens(Constraint, " .") of
        [Comparator, Major, Minor, Patch | _] ->
            {Comparator, {list_to_integer(Major),
                                list_to_integer(Minor),
                                list_to_integer(Patch)
                                }};
        _ ->
            ?ABORT("Invalid version constraint : ~p .\n", [Constraint])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Parsing the version (only allows X.Y.Z format)
-spec parse_version(string()) -> version().
%% @end
%%--------------------------------------------------------------------
parse_version(Vsn) ->
    case string:tokens(Vsn, ".") of
        [Major, Minor, Patch | _] ->
            {list_to_integer(Major),
            list_to_integer(Minor),
            list_to_integer(Patch)
            };
        _ ->
            ?ABORT("Invalid version for comparison with constraints : ~p .\n", [Vsn])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Compare two versions,
%% returns:  0 if equal
%%          -1 if left is smaller
%%           1 if left is bigger
-spec compare(version(), version()) -> -1 | 0 | 1.
%% @end
%%--------------------------------------------------------------------
compare(A, B) when A > B -> 1;
compare(A, B) when A < B -> -1;
compare(_, _) -> 0.
