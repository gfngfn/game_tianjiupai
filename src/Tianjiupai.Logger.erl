-module('Tianjiupai.Logger').
-export(['log_impl'/5, 'debug'/2, 'info'/2, 'warning'/2]).
    log_impl(Level, Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:log(Level, Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'debug'(S487Fmt, S488Args) -> fun(S489Filename, S490Line) -> 'Tianjiupai.Logger':'log_impl'('debug', S489Filename, S490Line, S487Fmt, S488Args) end.
'info'(S492Fmt, S493Args) -> fun(S494Filename, S495Line) -> 'Tianjiupai.Logger':'log_impl'('info', S494Filename, S495Line, S492Fmt, S493Args) end.
'warning'(S497Fmt, S498Args) -> fun(S499Filename, S500Line) -> 'Tianjiupai.Logger':'log_impl'('warning', S499Filename, S500Line, S497Fmt, S498Args) end.
