-module(fish).

-export([new/0,
         foo/1]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    case code:priv_dir(fish) of
        {error, bad_name} ->
            SoName = filename:join("../priv", fish);
        Dir ->
            SoName = filename:join(Dir, fish)
    end,
    erlang:load_nif(SoName, 0).

new() ->
    ?nif_stub.

foo(Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ok = foo(Ref).

-endif.
