-module('Tianjiupai.Logger').
-export(['log_impl'/5, 'debug'/2, 'info'/2, 'warning'/2]).
    log_impl(Level, Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:log(Level, Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'debug'(S495Fmt, S496Args) -> fun(S497Filename, S498Line) -> 'Tianjiupai.Logger':'log_impl'('debug', S497Filename, S498Line, S495Fmt, S496Args) end.
'info'(S500Fmt, S501Args) -> fun(S502Filename, S503Line) -> 'Tianjiupai.Logger':'log_impl'('info', S502Filename, S503Line, S500Fmt, S501Args) end.
'warning'(S505Fmt, S506Args) -> fun(S507Filename, S508Line) -> 'Tianjiupai.Logger':'log_impl'('warning', S507Filename, S508Line, S505Fmt, S506Args) end.
