-module('SesterlStdlib.MonitorRef').
-export(['monitor'/1, 'demonitor'/1, 'equal'/2]).
    monitor(Pid) ->
        erlang:monitor(process, Pid).
  
    demonitor(MRef) ->
        true = erlang:demonitor(MRef),
        ok.
  
    equal(MRef1, MRef2) ->
        MRef1 =:= MRef2.
  
