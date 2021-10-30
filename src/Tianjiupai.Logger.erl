-module('Tianjiupai.Logger').
-export(['log_impl'/5, 'debug'/2, 'info'/2, 'warning'/2]).
    log_impl(Level, Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:log(Level, Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'debug'(S478Fmt, S479Args) -> fun(S480Filename, S481Line) -> 'Tianjiupai.Logger':'log_impl'('debug', S480Filename, S481Line, S478Fmt, S479Args) end.
'info'(S483Fmt, S484Args) -> fun(S485Filename, S486Line) -> 'Tianjiupai.Logger':'log_impl'('info', S485Filename, S486Line, S483Fmt, S484Args) end.
'warning'(S488Fmt, S489Args) -> fun(S490Filename, S491Line) -> 'Tianjiupai.Logger':'log_impl'('warning', S490Filename, S491Line, S488Fmt, S489Args) end.
