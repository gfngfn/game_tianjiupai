-module('SesterlJson.Decode').
-export(['pure'/1, 'fail'/1, 'bool'/0, 'int'/0, 'string'/0, 'list_impl'/2, 'list'/1, 'bind'/2, 'field_impl'/2, 'field'/2, 'map'/2, 'map2'/3, 'decode_impl'/1, 'run'/2]).
'pure'(S344V) -> fun(S345Obj) -> {'ok', S344V} end.
'fail'(S347Msg) -> fun(S348Obj) -> {'error', {'failure', S347Msg}} end.
    bool() ->
        fun(Obj) ->
            if
                erlang:is_boolean(Obj) -> {ok, Obj};
                true                   -> {error, not_a_boolean}
            end
        end.
  
    int() ->
        fun(Obj) ->
            if
                erlang:is_integer(Obj) -> {ok, Obj};
                true                   -> {error, not_an_integer}
            end
        end.
  
    string() ->
        fun(Obj) ->
            if
                erlang:is_binary(Obj) -> {ok, Obj};
                true                  -> {error, not_a_string}
            end
        end.
  
'list_impl'(S353D, S354Objs) -> case 'SesterlStdlib.List':'foldl'(fun(S355Res, S356Obj) -> 'SesterlStdlib.Result':'bind'(S355Res, fun(S357Acc) -> 'SesterlStdlib.Result':'bind'(S353D(S356Obj), fun(S358V) -> {'ok', [S358V | S357Acc]} end) end) end, {'ok', []}, S354Objs) of {'ok', S359Acc} -> {'ok', 'SesterlStdlib.List':'reverse'(S359Acc)}; {'error', S360E} -> {'error', S360E} end.
    list(Decoder) ->
        fun(Obj) ->
            try
                list_impl(Decoder, Obj)
            catch
                _:_ -> {error, not_an_array}
            end
        end.
  
'bind'(S363D1, S364D2f) -> fun(S365Obj) -> case S363D1(S365Obj) of {'ok', S366V} -> begin S367 = S364D2f(S366V), S367(S365Obj) end; {'error', S368E} -> {'error', S368E} end end.
    field_impl(Key, Obj) ->
        try
            {ok, maps:get(Key, Obj)}
        catch
            _:_ -> error
        end.
  
'field'(S371Key, S372D) -> fun(S373Obj0) -> case 'SesterlJson.Decode':'field_impl'(S371Key, S373Obj0) of {'ok', S374Obj1} -> S372D(S374Obj1); 'error' -> {'error', {'missing_field', S371Key}} end end.
'map'(S376F, S377D) -> fun(S378Obj) -> 'SesterlStdlib.Result':'map'(S376F, S377D(S378Obj)) end.
'map2'(S380F, S381D1, S382D2) -> fun(S383Obj) -> 'SesterlStdlib.Result':'bind'(S381D1(S383Obj), fun(S384V1) -> 'SesterlStdlib.Result':'bind'(S382D2(S383Obj), fun(S385V2) -> {'ok', S380F(S384V1, S385V2)} end) end) end.
    decode_impl(Bin) ->
        try
            {ok, jsone:decode(Bin)}
        catch
            _:_ -> error
        end.
  
'run'(S388D, S389S) -> case 'SesterlJson.Decode':'decode_impl'(S389S) of {'ok', S390Obj} -> S388D(S390Obj); 'error' -> {'error', 'not_a_valid_json'} end.
