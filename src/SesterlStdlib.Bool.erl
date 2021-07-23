-module('SesterlStdlib.Bool').
-export(['not'/1]).
'not'(S26X) -> case S26X of true -> false; false -> true end.
