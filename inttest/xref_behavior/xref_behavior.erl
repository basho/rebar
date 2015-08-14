-module(xref_behavior).
-behavior(gen_xref_behavior).

% behavior-defined callbacks don't require xref_ignore
-export([init/1, handle/1]).

init(_Args) -> ok.

handle(_Atom) -> next_event.

