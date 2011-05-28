-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(DEPRECATED(Old, New, Opts, When),
        rebar_utils:deprecated(Old, New, Opts, When)).

-define(DEPRECATED(Old, New, When),
        rebar_utils:deprecated(Old, New, When)).
