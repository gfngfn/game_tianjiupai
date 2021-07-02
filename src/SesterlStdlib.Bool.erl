-module('SesterlStdlib.Bool').
-export(['not'/1]).
'not'(S22X) -> case S22X of true -> false; false -> true end.
