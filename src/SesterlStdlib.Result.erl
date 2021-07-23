-module('SesterlStdlib.Result').
-export(['map'/2, 'map_error'/2, 'bind'/2, 'from_option'/2]).
'map'(S29F, S30Res) -> case S30Res of {'ok', S31V} -> {'ok', S29F(S31V)}; {'error', S32E} -> {'error', S32E} end.
'map_error'(S34G, S35Res) -> case S35Res of {'ok', S36V} -> {'ok', S36V}; {'error', S37E} -> {'error', S34G(S37E)} end.
'bind'(S39Res, S40F) -> case S39Res of {'ok', S41V} -> S40F(S41V); {'error', S42E} -> {'error', S42E} end.
'from_option'(S44Opt, S45E) -> case S44Opt of {'ok', S46V} -> {'ok', S46V}; 'error' -> {'error', S45E} end.
