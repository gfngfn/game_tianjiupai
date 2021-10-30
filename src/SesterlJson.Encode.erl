-module('SesterlJson.Encode').
-export(['bool'/1, 'int'/1, 'string'/1, 'list'/1, 'object'/1, 'object_from_list'/1, 'run'/1]).
'bool'(S431B) -> 'SesterlStdlib.RawValue':'forget'(S431B).
'int'(S433N) -> 'SesterlStdlib.RawValue':'forget'(S433N).
'string'(S435S) -> 'SesterlStdlib.RawValue':'forget'(S435S).
'list'(S437Vs) -> 'SesterlStdlib.RawValue':'forget'(S437Vs).
'object'(S439Map) -> 'SesterlStdlib.RawValue':'forget'(S439Map).
'object_from_list'(S441List) -> begin S442Map = 'SesterlStdlib.RawMap':'from_list'(S441List), 'SesterlStdlib.RawValue':'forget'(S442Map) end.
    run(Obj) ->
        jsone:encode(Obj).
  
