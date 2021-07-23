-module('SesterlStdlib.RawMap').
-export(['new'/0, 'put'/3, 'find'/2, 'remove'/2, 'to_list'/1, 'from_list'/1, 'merge'/2, 'merge_with'/3, 'map'/2, 'fold'/3]).
    new() ->
        #{}.
  
    put(Key, Value, Map) ->
        maps:put(Key, Value, Map).
  
    find(Key, Map) ->
        maps:find(Key, Map).
  
    remove(Key, Map) ->
        maps:remove(Key, Map).
  
    to_list(Map) ->
        maps:to_list(Map).
  
    from_list(Kvs) ->
        maps:from_list(Kvs).
  
    merge(Map1, Map2) ->
        maps:merge(Map1, Map2).
  
    merge_with(Combiner, Map1, Map2) ->
        maps:merge_with(Combiner, Map1, Map2).
  
    map(Fun, Map) ->
        maps:map(Fun, Map).
  
    fold(Fun, Init, Map) ->
        maps:fold(Fun, Init, Map).
  
