-module('Tianjiupai.Logger').
-export(['log_impl'/5, 'debug'/2, 'info'/2, 'warning'/2]).
    log_impl(Level, Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:log(Level, Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'debug'(S481Fmt, S482Args) -> fun(S483Filename, S484Line) -> 'Tianjiupai.Logger':'log_impl'('debug', S483Filename, S484Line, S481Fmt, S482Args) end.
'info'(S486Fmt, S487Args) -> fun(S488Filename, S489Line) -> 'Tianjiupai.Logger':'log_impl'('info', S488Filename, S489Line, S486Fmt, S487Args) end.
'warning'(S491Fmt, S492Args) -> fun(S493Filename, S494Line) -> 'Tianjiupai.Logger':'log_impl'('warning', S493Filename, S494Line, S491Fmt, S492Args) end.
