-module('SesterlStdlib.Float').
-export(['from_int'/1, 'round'/1, 'truncate'/1]).
    from_int(N) ->
        erlang:float(N).
  
    round(X) ->
        erlang:round(X).
  
    truncate(X) ->
        erlang:trunc(X).
  
