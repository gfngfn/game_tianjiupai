-module('SesterlStdlib.Binary').
-export(['equal'/2, 'from_int'/1, 'from_list'/1, 'to_list'/1, 'to_list_sub'/3, 'at'/2]).

    equal(B1, B2) ->
        B1 =:= B2.
  

    from_int(N) ->
        erlang:integer_to_binary(N).
  

    from_list(Chars) ->
        erlang:list_to_binary(Chars).
  

    to_list(Bin) ->
        erlang:binary_to_list(Bin).
  

    to_list_sub(Bin, Len, Pos) ->
    %% Labeled mandatory arguments are in the alphabetical order.
        try
            Chars = binary:bin_to_list(Bin, Pos, Len),
            {ok, Chars}
        catch
            _:_ ->
                error
        end.
  

    at(Bin, Pos) ->
        try
            Char = binary:at(Bin, Pos),
            {ok, Char}
        catch
            _:_ ->
                error
        end.
  
