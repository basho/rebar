%%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%%-------------------------------------------------------------------
%%% @author Tuncer Ayaz
%%% @copyright 2015, Tuncer Ayaz
%%% @doc
%%% memoization server
%%% @end
%%%-------------------------------------------------------------------
%%%
%%% Copyright (c) 2015 Tuncer Ayaz
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% rebar-specific modifications:
%% 1. rename to rmemo.erl
%% 2. add support for R13 (see ets_tab/0)

-module(rmemo).

-behaviour(gen_server).

%% API
-export(
   [
    start/0,
    start_link/0,
    stop/0,
    call/2,
    call/3
   ]).

%% gen_server callbacks
-export(
   [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(state,
        {
          ets_tab :: ets:tab()
        }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Start the server
%% @end
%%--------------------------------------------------------------------
-type reason() :: term().
-type error() :: {error, reason()}.
-type start_res() :: {ok, pid()} | 'ignore' | error().
-spec start() -> start_res().
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Start the server
%% @end
%%--------------------------------------------------------------------
-type start_link_res() :: start_res().
-spec start_link() -> start_link_res().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Call function and memoize result
%%
%% Instead of
%%
%% <code>Res = Fun(A1, A2, [List1])</code>
%%
%% you call
%%
%% <code>Res = memo:call(Fun, [A1, A2, [List1]])</code>
%%
%% or instead of
%%
%% <code>
%% Res = mod:expensive_function(A1, A2, [List1])
%% </code>
%%
%% you call
%%
%% <code>
%% Res = memo:call(fun mod:expensive_function/3, [A1, A2, [List1]])
%% </code>
%%
%% and any subsequent call will fetch the cached result and avoid the
%% computation.
%%
%% This is of course only useful for expensive computations that are
%% known to produce the same result given same arguments. It's worth
%% mentioning that your call should be side-effect free, as naturally
%% those won't be replayed.
%%
%% @end
%%--------------------------------------------------------------------
-type fun_args() :: list().
-spec call(fun(), fun_args()) -> term().
call(F, A) ->
    call_1({F, A}).

%%--------------------------------------------------------------------
%% @doc
%% Call function and memoize result
%%
%% Instead of
%%
%% <code>Res = mod:expensive_function(A1, A2, [List1])</code>
%%
%% you call
%%
%% <code>Res = memo:call(mod, expensive_function, [A1, A2, [List1]])</code>
%%
%% and any subsequent call will fetch the cached result and avoid the
%% computation.
%%
%% This is of course only useful for expensive computations that are
%% known to produce the same result given same arguments. It's worth
%% mentioning that your call should be side-effect free, as naturally
%% those won't be replayed.%%
%%
%% @end
%%--------------------------------------------------------------------
%% fun() is not just the name of a fun, so we define an alias for
%% atom() for call(M, F, A).
-type fun_name() :: atom().
-spec call(module(), fun_name(), fun_args()) -> term().
call(M, F, A) when is_list(A) ->
    call_1({M, F, A}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialize the server
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {ok,
     #state{
        ets_tab = ets_tab()
       }
    }.

-spec ets_tab() -> ets:tab().
ets_tab() ->
    Concurrency =
        %% read_concurrency was added in R14. If we're running R13,
        %% do not try to use it.
        case erlang:system_info(version) =< "5.8.3" of
            true ->
                [];
            false ->
                [{read_concurrency, true}]
        end,
    ets:new(
      ?TABLE,
      [
       named_table,
       protected,
       set
      ]
      ++ Concurrency
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle call messages
%% @end
%%--------------------------------------------------------------------
handle_call({save, Key, Res}, _From, State) ->
    {reply, save(Key, Res), State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-type call() :: {module(), fun_name(), fun_args()} | {fun(), fun_args()}.
-spec call_1(call()) -> term().
call_1(Call) ->
    Key = key(Call),
    case ets:lookup(?TABLE, Key) of
        [] ->
            Res = apply(Call),
            true = gen_server:call(?SERVER, {save, Key, Res}, infinity),
            Res;
        [{Key, Mem}] ->
            Mem
    end.

-type key_args() :: call().
-type key() :: non_neg_integer().
-spec key(key_args()) -> key().
key(Call) ->
    erlang:phash2(Call).

-spec apply(call()) -> term().
apply({F, A}) ->
    erlang:apply(F, A);
apply({M, F, A}) ->
    erlang:apply(M, F, A).

-type val() :: term().
-spec save(key(), val()) -> true.
save(K, V) ->
    ets:insert(?TABLE, {K, V}).
