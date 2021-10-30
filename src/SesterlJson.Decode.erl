-module('SesterlJson.Decode').
-export(['pure'/1, 'fail'/1, 'bool'/0, 'int'/0, 'string'/0, 'list_impl'/2, 'list'/1, 'bind'/2, 'field_impl'/2, 'field'/2, 'map'/2, 'map2'/3, 'decode_impl'/1, 'run'/2]).
'pure'(S382V) -> fun(S383Obj) -> {'ok', S382V} end.
'fail'(S385Msg) -> fun(S386Obj) -> {'error', {'failure', S385Msg}} end.
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
  
'list_impl'(S391D, S392Objs) -> case 'SesterlStdlib.List':'foldl'(fun(S393Res, S394Obj) -> 'SesterlStdlib.Result':'bind'(S393Res, fun(S395Acc) -> 'SesterlStdlib.Result':'bind'(S391D(S394Obj), fun(S396V) -> {'ok', [S396V | S395Acc]} end) end) end, {'ok', []}, S392Objs) of {'ok', S397Acc} -> {'ok', 'SesterlStdlib.List':'reverse'(S397Acc)}; {'error', S398E} -> {'error', S398E} end.
    list(Decoder) ->
        fun(Obj) ->
            try
                list_impl(Decoder, Obj)
            catch
                _:_ -> {error, not_an_array}
            end
        end.
  
'bind'(S401D1, S402D2f) -> fun(S403Obj) -> case S401D1(S403Obj) of {'ok', S404V} -> begin S405 = S402D2f(S404V), S405(S403Obj) end; {'error', S406E} -> {'error', S406E} end end.
    field_impl(Key, Obj) ->
        try
            {ok, maps:get(Key, Obj)}
        catch
            _:_ -> error
        end.
  
'field'(S409Key, S410D) -> fun(S411Obj0) -> case 'SesterlJson.Decode':'field_impl'(S409Key, S411Obj0) of {'ok', S412Obj1} -> S410D(S412Obj1); 'error' -> {'error', {'missing_field', S409Key}} end end.
'map'(S414F, S415D) -> fun(S416Obj) -> 'SesterlStdlib.Result':'map'(S414F, S415D(S416Obj)) end.
'map2'(S418F, S419D1, S420D2) -> fun(S421Obj) -> 'SesterlStdlib.Result':'bind'(S419D1(S421Obj), fun(S422V1) -> 'SesterlStdlib.Result':'bind'(S420D2(S421Obj), fun(S423V2) -> {'ok', S418F(S422V1, S423V2)} end) end) end.
    decode_impl(Bin) ->
        try
            {ok, jsone:decode(Bin)}
        catch
            _:_ -> error
        end.
  
'run'(S426D, S427S) -> case 'SesterlJson.Decode':'decode_impl'(S427S) of {'ok', S428Obj} -> S426D(S428Obj); 'error' -> {'error', 'not_a_valid_json'} end.
