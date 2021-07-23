-module('SesterlTesting.Testing').
-export(['it'/2, 'perform'/2, 'list'/1, 'equal'/2]).
    it(Title, F) ->
        {Title, F}.
  
    perform(Title, F) ->
        {Title, F}.
  
    list(Ts) ->
        Ts.
  
    equal(Expect, Got) ->
        fun(FileName, LineNumber) ->
            case Got of
                Expect ->
                    ok;
                _ ->
                    Msg = lists:flatten(io_lib:format("('~s' at line ~w)", [FileName, LineNumber])),
                    erlang:error({assertEqual_failed, [
                        {module, erlang:binary_to_atom(FileName)},
                        {line, LineNumber},
                        {expression, Msg},
                        {expected, Expect},
                        {value, Got}]})
            end
        end.
  
