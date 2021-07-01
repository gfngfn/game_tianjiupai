-module('SesterlStdlib.Option').
-export(['get'/2, 'get_or_invoke'/2, 'map'/2, 'bind'/2]).
'get'(S66X, S67D) -> case S66X of 'error' -> S67D; {'ok', S68V} -> S68V end.
'get_or_invoke'(S70X, S71Df) -> case S70X of 'error' -> S71Df(); {'ok', S72V} -> S72V end.
'map'(S74F, S75X) -> case S75X of 'error' -> 'error'; {'ok', S76V} -> {'ok', S74F(S76V)} end.
'bind'(S78X, S79F) -> case S78X of 'error' -> 'error'; {'ok', S80V} -> S79F(S80V) end.
