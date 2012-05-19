-module({{id}}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% Required OTP Exports
%% ------------------------------------------------------------------
-export([init/1]).
-export([
    system_code_change/4, system_continue/3,
    system_terminate/4, write_debug/3
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, Debug, []).

loop(Parent, Debug, State) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(
                Request, From, Parent, ?MODULE, Debug, State
            )
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

system_continue(Parent, Debug, State) ->
    loop(Parent, Debug, State).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

