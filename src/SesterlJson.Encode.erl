-module('SesterlJson.Encode').
-export(['bool'/1, 'int'/1, 'string'/1, 'list'/1, 'object'/1, 'object_from_list'/1, 'run'/1]).
'bool'(S393B) -> 'SesterlStdlib.RawValue':'forget'(S393B).
'int'(S395N) -> 'SesterlStdlib.RawValue':'forget'(S395N).
'string'(S397S) -> 'SesterlStdlib.RawValue':'forget'(S397S).
'list'(S399Vs) -> 'SesterlStdlib.RawValue':'forget'(S399Vs).
'object'(S401Map) -> 'SesterlStdlib.RawValue':'forget'(S401Map).
'object_from_list'(S403List) -> begin S404Map = 'SesterlStdlib.RawMap':'from_list'(S403List), 'SesterlStdlib.RawValue':'forget'(S404Map) end.
    run(Obj) ->
        jsone:encode(Obj).
  
