-module('SesterlStdlib.Bool').
-export(['not'/1]).
'not'(S23X) -> case S23X of true -> false; false -> true end.
