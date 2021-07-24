-module('Tianjiupai.Logger').
-export(['log_impl'/5, 'info'/2, 'warning'/2]).
    log_impl(Level, Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:log(Level, Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'info'(S434Fmt, S435Args) -> fun(S436Filename, S437Line) -> 'Tianjiupai.Logger':'log_impl'('info', S436Filename, S437Line, S434Fmt, S435Args) end.
'warning'(S439Fmt, S440Args) -> fun(S441Filename, S442Line) -> 'Tianjiupai.Logger':'log_impl'('info', S441Filename, S442Line, S439Fmt, S440Args) end.
