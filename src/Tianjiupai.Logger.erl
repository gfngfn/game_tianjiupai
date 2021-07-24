-module('Tianjiupai.Logger').
-export(['info_impl'/4, 'info'/2, 'warning_impl'/4, 'warning'/2]).
    info_impl(Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:info(Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'info'(S434Fmt, S435Args) -> fun(S436Filename, S437Line) -> 'Tianjiupai.Logger':'info_impl'(S436Filename, S437Line, S434Fmt, S435Args) end.
    warning_impl(Filename, Line, FormatAndArity, Args) ->
        {Format, _} = FormatAndArity,
        FilenameStr = erlang:binary_to_list(Filename),
        logger:warning(Format, erlang:tuple_to_list(Args), #{ file => FilenameStr, line => Line }).
  
'warning'(S440Fmt, S441Args) -> fun(S442Filename, S443Line) -> 'Tianjiupai.Logger':'warning_impl'(S442Filename, S443Line, S440Fmt, S441Args) end.
