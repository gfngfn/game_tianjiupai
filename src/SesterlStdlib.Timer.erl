-module('SesterlStdlib.Timer').
-export(['send_after'/3, 'send_after_self'/2, 'cancel'/1]).
    send_after(Time, Pid, Msg) ->
        timer:send_after(Time, Pid, Msg).
  
    send_after_self(Time, Msg) ->
        timer:send_after(Time, Msg).
  
    cancel(TRef) ->
        Result = timer:cancel(TRef),
        case Result of
            {ok, cancel}     -> {ok, ok};
            {error, _} = Err -> Err
        end.
  
