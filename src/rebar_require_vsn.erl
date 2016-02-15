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
%%
%% -------------------------------------------------------------------

-module(rebar_require_vsn).

-include("rebar.hrl").

-export([compile/2,
         eunit/2,
         version_tuple/1]).

%% for internal use only
-export([info/2,
         version_tuple/3]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _) ->
    check_versions(Config).

eunit(Config, _) ->
    check_versions(Config).

%% ====================================================================
%% Internal functions
%% ====================================================================

info(help, compile) ->
    info_help();
info(help, eunit) ->
    info_help().

info_help() ->
    ?CONSOLE(
       "Check required ERTS or OTP release version.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n",
       [
        {require_erts_vsn, ".*"},
        {require_otp_vsn, ".*"},
        {require_min_otp_vsn, ".*"}
       ]).

check_versions(Config) ->
    ShouldAbort = case rebar_config:get_xconf(Config, keep_going, false) of
                      true -> keep_going;
                      false -> abort
                  end,
    ErtsRegex = rebar_config:get(Config, require_erts_vsn, ".*"),
    ReOpts = [{capture, none}],
    case re:run(erlang:system_info(version), ErtsRegex, ReOpts) of
        match ->
            ?DEBUG("Matched required ERTS version: ~s -> ~s\n",
                   [erlang:system_info(version), ErtsRegex]);
        nomatch ->
            maybe_abort(
              ShouldAbort,
              "ERTS version ~s does not match required regex ~s\n",
              [erlang:system_info(version), ErtsRegex])
    end,

    OtpRegex = rebar_config:get(Config, require_otp_vsn, ".*"),
    case re:run(erlang:system_info(otp_release), OtpRegex, ReOpts) of
        match ->
            ?DEBUG("Matched required OTP release: ~s -> ~s\n",
                   [erlang:system_info(otp_release), OtpRegex]);
        nomatch ->
            maybe_abort(
              ShouldAbort,
              "OTP release ~s does not match required regex ~s\n",
              [erlang:system_info(otp_release), OtpRegex])
    end,

    case rebar_config:get(Config, require_min_otp_vsn, undefined) of
        undefined -> ?DEBUG("Min OTP version unconfigured~n", []);
        MinOtpVsn ->
            {MinMaj, MinMin, MinPatch} = version_tuple(ShouldAbort, MinOtpVsn,
                                             "configured"),
            {OtpMaj, OtpMin, OtpPatch} = version_tuple(ShouldAbort,
                                             erlang:system_info(otp_release),
                                             "OTP Release"),
            case {OtpMaj, OtpMin, OtpPatch} >= {MinMaj, MinMin, MinPatch} of
                true ->
                    ?DEBUG("~s satisfies the requirement for vsn ~s~n",
                           [erlang:system_info(otp_release),
                            MinOtpVsn]);
                false ->
                    maybe_abort(
                      ShouldAbort,
                      "OTP release ~s or later is required, you have: ~s~n",
                      [MinOtpVsn,
                       erlang:system_info(otp_release)])
            end
    end.

version_tuple(OtpRelease) ->
    version_tuple(keep_going, OtpRelease, "").

version_tuple(ShouldAbort, OtpRelease, Type) ->
    case re:run(OtpRelease, "R?(\\d+)B?\.?(\\d+)?\.?-?(\\d+)?", [{capture, all, list}]) of
        {match, [_Full, Maj, Min, Patch]} ->
            {list_to_integer(Maj), list_to_integer(Min), list_to_integer(Patch)};
        {match, [_Full, Maj, Min]} ->
            {list_to_integer(Maj), list_to_integer(Min), 0};
        {match, [_Full, Maj]} ->
            {list_to_integer(Maj), 0, 0};
        nomatch ->
            maybe_abort(ShouldAbort,
                        "Cannot parse ~s version string: ~s~n",
                        [Type, OtpRelease])
    end.

maybe_abort(abort, Format, Data) ->
    ?ABORT(Format, Data);
maybe_abort(keep_going, Format, Data) ->
    ?ERROR(Format, Data).
