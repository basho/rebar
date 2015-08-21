-module(gen_xref_behavior).

-export([behaviour_info/1]). -ignore_xref([{behaviour_info, 1}]).

behaviour_info(callbacks) ->
    [{init,1}, {handle, 1}];
behaviour_info(_Other) ->
    undefined.

