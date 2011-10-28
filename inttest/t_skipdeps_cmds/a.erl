-module(a).
-compile(export_all).

hello() ->
    io:format("~s\n", [b:hello()]).

