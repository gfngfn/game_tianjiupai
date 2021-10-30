-module('SesterlStdlib.Result').
-export(['map'/2, 'map_error'/2, 'bind'/2, 'from_option'/2]).
'map'(S26F, S27Res) -> case S27Res of {'ok', S28V} -> {'ok', S26F(S28V)}; {'error', S29E} -> {'error', S29E} end.
'map_error'(S31G, S32Res) -> case S32Res of {'ok', S33V} -> {'ok', S33V}; {'error', S34E} -> {'error', S31G(S34E)} end.
'bind'(S36Res, S37F) -> case S36Res of {'ok', S38V} -> S37F(S38V); {'error', S39E} -> {'error', S39E} end.
'from_option'(S41Opt, S42E) -> case S41Opt of {'ok', S43V} -> {'ok', S43V}; 'error' -> {'error', S42E} end.
