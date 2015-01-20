#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% This file is left for backward-compatibility.
%% You, probably, shouldn't include it to new projects.


main([NodeName, Cookie, ReleasePackage]) ->
    io:format("WARNING: 'install_upgrade.escript' is deprecated! "
              "Use 'nodetool upgrade' instead.~n"),
    NodeRoot = filename:dirname(filename:dirname(escript:script_name())),
    NodeTool = which_nodetool(NodeRoot),
    process_flag(trap_exit, true),
    Port = erlang:open_port(
             {spawn_executable, NodeTool},
             [{args, ["-sname", NodeName,
                      "-setcookie", Cookie,
                      "upgrade", ReleasePackage]},
              binary, exit_status, use_stdio, stderr_to_stdout, hide]),
    port_loop(Port);
main(_) ->
    halt(1).


which_nodetool(NodeRoot) ->
    %% ${RELEASE_ROOT}/
    %%   bin/install_upgrade.escript
    %%   bin/nodetool ?
    %%   erts-<erts_ver>/bin/nodetool ?
    %%   releases/<app_ver>/nodetool ?
    %%   releases/start_erl.data
    {ok, Content} = file:read_file(filename:join([NodeRoot, "releases", "start_erl.data"])),
    [ErtsVsn, AppVsn] = binary:split(Content, <<" ">>),
    Probes = [
              filename:join([NodeRoot, "bin", "nodetool"]),
              filename:join([NodeRoot, <<"erts-", ErtsVsn/binary>>, "bin", "nodetool"]),
              filename:join([NodeRoot, "releases", AppVsn, "bin", "nodetool"])
             ],
    case lists:dropwhile(fun(Path) -> not filelib:is_regular(Path) end, Probes) of
        [] ->
            io:format("ERROR: can't find 'nodetool' in ~p.~n", [Probes]),
            halt(2);
        [Path | _] ->
            Path
    end.

port_loop(Port) ->
    receive
        {Port, {data, Data}} ->
            io:put_chars(Data),
            port_loop(Port);
        {Port, {exit_status, Status}} ->
            halt(Status)
    end.
