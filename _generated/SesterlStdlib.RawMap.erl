-module('SesterlStdlib.RawMap').
-export(['new'/0, 'put'/3, 'find'/2, 'remove'/2, 'to_list'/1, 'merge'/2, 'map'/2]).

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
  

    merge(Map1, Map2) ->
        maps:merge(Map1, Map2).
  

    map(Fun, Map) ->
        maps:map(Fun, Map).
  
