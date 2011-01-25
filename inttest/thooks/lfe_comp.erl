%% a dummy lfe_comp module, just so we can test the hooks! 
-module(lfe_comp).

-compile(export_all).

file(_Source, _Opts) ->
    {ok, ignored, []}.
