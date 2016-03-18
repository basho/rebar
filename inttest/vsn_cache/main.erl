-module(main).
-behaviour(application).
     
-export([start/0,start/1,start/2,stop/1]).

start() ->
	start(permanent).
start(_Restart) ->
	ok.
start(_Type,_Args) ->
	ok.
stop(_State) ->
	ok.
