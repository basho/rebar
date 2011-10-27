-module(b).
-compile(export_all).

hello() ->
    c:hello().
