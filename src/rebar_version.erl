%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2012 Thijs Terlouw (thijsterlouw@spilgames.com)
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
         
%% New detailed version support
%% example: [ ">= 1.0.0", "< 2.0.0" ]
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


%% Compare two versions, 
%% returns:  0 if equal
%%          -1 if left is smaller
%%           1 if left is bigger
compare({MajorA,MinorA,PatchA}, {MajorB,MinorB,PatchB}) ->
    if
        MajorA =:= MajorB andalso MinorA =:= MinorB andalso PatchA =:= PatchB ->
           0;
        true->
            if 
                MajorA < MajorB ->
                   -1;
                MajorA =:= MajorB ->
                    if
                        MinorA < MinorB ->
                            -1;
                        MinorA =:= MinorB ->
                            if
                                PatchA < PatchB ->
                                    -1;
                                true ->
                                    1
                            end;
                        true ->
                            1
                    end;
                true ->
                    1
            end
    end.


check_constraints(Vsn, Constraints) ->
    %TODO implement actual constraint checks
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


%% Parsing the constraints. Supported comparators are:
%% >= > = <= and <
%% Example:  [ ">= 1.0.8", "< 1.1.0" ]
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

%% Parsing the version (only allows X.Y.Z format)
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