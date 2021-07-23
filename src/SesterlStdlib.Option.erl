-module('SesterlStdlib.Option').
-export(['get'/2, 'get_or_invoke'/2, 'map'/2, 'bind'/2]).
'get'(S70X, S71D) -> case S70X of 'error' -> S71D; {'ok', S72V} -> S72V end.
'get_or_invoke'(S74X, S75Df) -> case S74X of 'error' -> S75Df(); {'ok', S76V} -> S76V end.
'map'(S78F, S79X) -> case S79X of 'error' -> 'error'; {'ok', S80V} -> {'ok', S78F(S80V)} end.
'bind'(S82X, S83F) -> case S82X of 'error' -> 'error'; {'ok', S84V} -> S83F(S84V) end.
