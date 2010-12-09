%% @author Author <author@domain.com>
%% @copyright YYYY Author

%% @doc {{appid}} startup code

-module({{appid}}).
-author('Author').

-export([start/0, start_link/0, stop/0]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the {{appid}} app for inclusion in a supervisor tree
start_link() ->
  ensure_started(crypto),
  {{appid}}_sup:start_link().

%% @spec start() -> ok
%% @doc Start the {{appid}} server.
start() ->
  ensure_started(crypto),
  application:start({{appid}}).

%% @spec stop() -> ok
%% @doc Stop the {{appid}} server.
stop() ->
  Res = application:stop({{appid}}),
  application:stop(crypto),
  Res.

%% ===================================================================
%% Internal Functions
%% ===================================================================

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    { error, { already_started, App } } ->
      ok
  end.

