%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

{application, rebar,
 [{description, "Rebar: Erlang Build Tool"},
  {vsn, "2.6.1"},
  {modules, [ rebar,
              rebar_abnfc_compiler,
              rebar_app_utils,
              rebar_appups,
              rebar_asn1_compiler,
              rebar_dia_compiler,
              rebar_base_compiler,
              rebar_cleaner,
              rebar_config,
              rebar_core,
              rebar_cover_utils,
              rebar_ct,
              rebar_deps,
              rebar_dialyzer,
              rebar_edoc,
              rebar_erlc_compiler,
              rebar_erlydtl_compiler,
              rebar_escripter,
              rebar_eunit,
              rebar_file_utils,
              rebar_lfe_compiler,
              rebar_log,
              rebar_neotoma_compiler,
              rebar_otp_app,
              rebar_otp_appup,
              rebar_port_compiler,
              rebar_proto_compiler,
              rebar_protobuffs_compiler,
              rebar_proto_gpb_compiler,
              rebar_qc,
              rebar_rel_utils,
              rebar_reltool,
              rebar_require_vsn,
              rebar_shell,
              rebar_subdirs,
              rebar_templater,
              rebar_upgrade,
              rebar_utils,
              rebar_xref,
              rebar_metacmds,
              rebar_getopt,
              rebar_mustache,
              rmemo ]},
  {registered, []},
  {applications,
   [
    kernel,
    stdlib,
    sasl,
    compiler,
    crypto,
    syntax_tools,
    tools,
    eunit,
    reltool,
    dialyzer,
    asn1,
    snmp,
    edoc
   ]},
  {env, [
         %% Default log level
         {log_level, warn},

         %% Log colored
         {log_colored, uncolored},

         %% any_dir processing modules
         {any_dir_modules, [
                            rebar_require_vsn,
                            rebar_deps,
                            rebar_subdirs,
                            rebar_templater,
                            rebar_cleaner
                           ]},

         %% Dir specific processing modules
         {modules, [
                    {app_dir, [
                               rebar_abnfc_compiler,
                               rebar_proto_compiler,
                               rebar_protobuffs_compiler,
                               rebar_proto_gpb_compiler,
                               rebar_neotoma_compiler,
                               rebar_asn1_compiler,
                               rebar_dia_compiler,
                               rebar_dialyzer,
                               rebar_erlc_compiler,
                               rebar_lfe_compiler,
                               rebar_erlydtl_compiler,
                               rebar_port_compiler,
                               rebar_otp_app,
                               rebar_otp_appup,
                               rebar_ct,
                               rebar_eunit,
                               rebar_qc,
                               rebar_escripter,
                               rebar_edoc,
                               rebar_shell,
                               rebar_xref,
                               rebar_metacmds
                              ]},

                    {rel_dir, [
                               rebar_appups,
                               rebar_reltool,
                               rebar_upgrade
                              ]}
                   ]},
         {recursive_cmds, [
                           'check-deps',
                           compile,
                           'delete-deps',
                           'get-deps',
                           'list-deps',
                           'update-deps'
                          ]}
        ]}
 ]}.
