-module('SesterlStdlib.Option').
-export(['get'/2, 'get_or_invoke'/2, 'map'/2, 'bind'/2]).
'get'(S75X, S76D) -> case S75X of 'error' -> S76D; {'ok', S77V} -> S77V end.
'get_or_invoke'(S79X, S80Df) -> case S79X of 'error' -> S80Df(); {'ok', S81V} -> S81V end.
'map'(S83F, S84X) -> case S84X of 'error' -> 'error'; {'ok', S85V} -> {'ok', S83F(S85V)} end.
'bind'(S87X, S88F) -> case S87X of 'error' -> 'error'; {'ok', S89V} -> S88F(S89V) end.
