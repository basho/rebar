%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_dia_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(REBAR_SCRIPT, "../rebar").

-define(TMP_DIR, "dia_project/").

dia_test_() ->
    case supported_otp_version() of
        true -> 
            {"Test the dia compiler",
             setup, fun() -> setup_project(), rebar("compile") end,
             fun teardown/1,
             fun(RebarOut) ->
                 [{"Check ebin is created",
                   ?_assert(filelib:is_dir("ebin") =:= true)},

                  {"Check include is created",
                   ?_assert(filelib:is_dir("include") =:= true)},
                  
                  {"Check dia/a.dia is compiled",
                   ?_assert(string:str(RebarOut, "Compiled dia/a.dia") /= 0)},

                  {"Check dia/b.dia is compiled",
                   ?_assert(string:str(RebarOut, "Compiled dia/b.dia") /= 0)}
                 ]
             end};
        false -> {setup, fun() -> ok end, []}
    end.


%% ====================================================================
%% Setup and Teardown
%% ====================================================================

-define(myapp_dia_a,
        ["@id     0\n",
         "@name   a\n",
         "@vendor 1 ABC\n",
         "@avp_types\n",
         "   IMSI 1 UTF8String V"
        ]).

-define(myapp_dia_b,
        ["@id     2\n",
         "@name   b\n",
         "@vendor 2 ABC\n",
         "@inherits a\n",
         "@avp_types\n",
         "   IMEIV 900 OctetString MV"
        ]).

-define(myapp_rebarconfig,
        ["{dia_first_files, [\"dia/a.dia\"]}.\n",
         "{dia_opts, []}.\n"
        ]).

supported_otp_version() ->
    Min = rebar_require_vsn:version_tuple(keep_going, "15", "configured"),
    Otp = rebar_require_vsn:version_tuple(keep_going, 
                                          erlang:system_info(otp_release),
                                          "OTP Release"),
    Otp >= Min.


setup_environment() ->
    ok = file:make_dir(?TMP_DIR),
    prepare_rebar_script(),
    ok = file:set_cwd(?TMP_DIR).

prepare_project() ->
    setup_environment(),
    rebar("create-app appid=myapp").


setup_project() ->
    prepare_project(),
    ok = file:make_dir("dia"),
    ok = file:write_file("dia/a.dia", ?myapp_dia_a),
    ok = file:write_file("dia/b.dia", ?myapp_dia_b),
    ok = file:write_file("rebar.config", ?myapp_rebarconfig).


teardown(_) ->
    ok = file:set_cwd(".."),
    ok = remove_tmp_dir().

remove_tmp_dir() ->
    ok = rebar_file_utils:rm_rf(?TMP_DIR).

%% ====================================================================
%% Helper Functions
%% ====================================================================

prepare_rebar_script() ->
    Rebar = ?TMP_DIR ++ "rebar",
    {ok, _} = file:copy(?REBAR_SCRIPT, Rebar),
    case os:type() of
        {unix, _} ->
            [] = os:cmd("chmod u+x " ++ Rebar);
        {win32, _} ->
            {ok, _} = file:copy(?REBAR_SCRIPT ++ ".cmd",
                                ?TMP_DIR ++ "rebar.cmd")
    end.

rebar() ->
    rebar([]).

rebar(Args) when is_list(Args) ->
    Out = os:cmd(filename:nativename("./rebar") ++ " " ++ Args),
    Out.
