-module('SesterlStdlib.Result').
-export(['map'/2, 'map_error'/2, 'bind'/2, 'from_option'/2]).
'map'(S25F, S26Res) -> case S26Res of {'ok', S27V} -> {'ok', S25F(S27V)}; {'error', S28E} -> {'error', S28E} end.
'map_error'(S30G, S31Res) -> case S31Res of {'ok', S32V} -> {'ok', S32V}; {'error', S33E} -> {'error', S30G(S33E)} end.
'bind'(S35Res, S36F) -> case S35Res of {'ok', S37V} -> S36F(S37V); {'error', S38E} -> {'error', S38E} end.
'from_option'(S40Opt, S41E) -> case S40Opt of {'ok', S42V} -> {'ok', S42V}; 'error' -> {'error', S41E} end.
