-module('Tianjiupai.Logger').
-export(['log_impl'/5, 'info'/2, 'warning'/2]).
    log_impl(Filename, Line, LogLevel, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        TimestampMs = erlang:system_time(millisecond),
        TimestampString = calendar:system_time_to_rfc3339(TimestampMs, [{unit, millisecond}]),
        Self = self(),
        io:format(
            "~s:~p ~s ~p [~s] " ++ Format ++ "~n",
            [Filename, Line, TimestampString, Self, LogLevel] ++ erlang:tuple_to_list(Args)).
  
'info'(S434Fmt, S435Args) -> fun(S436Filename, S437Line) -> 'Tianjiupai.Logger':'log_impl'(S436Filename, S437Line, "info", S434Fmt, S435Args) end.
'warning'(S439Fmt, S440Args) -> fun(S441Filename, S442Line) -> 'Tianjiupai.Logger':'log_impl'(S441Filename, S442Line, "warning", S439Fmt, S440Args) end.
