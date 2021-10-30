-module('SesterlStdlib.StopReason').
-export(['normal'/0, 'shutdown'/1, 'abort'/1, 'lift'/1]).
    normal() ->
        normal.
  
    shutdown({ok, Info}) -> {shutdown, Info};
    shutdown(error)      -> shutdown.
  
    abort(X) -> {'$sesterl_abort', X}.
  
    lift(Reason) ->
        case Reason of
            normal                -> normal;
            shutdown              -> {shutdown, error};
            {shutdown, Info}      -> {shutdown, {ok, Info}};
            {'$sesterl_abort', X} -> {abort, X};
            _                     -> {other, Reason}
        end.
  
