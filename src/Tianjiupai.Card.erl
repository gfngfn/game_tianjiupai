-module('Tianjiupai.Card').
-export(['wen_greater'/2, 'wen_equal'/2, 'wu_number_greater'/2, 'wu_greater'/2, 'wu_equal'/2, 'big_greater'/2, 'sort'/1, 'big_to_wen_and_wu'/1, 'wen_and_wu_to_big'/2]).
'wen_greater'(S919Wen1, S920Wen2) -> (S919Wen1 > S920Wen2).
'wen_equal'(S922Wen1, S923Wen2) -> (S922Wen1 == S923Wen2).
'wu_number_greater'(S925Wunum1, S926Wunum2) -> (S925Wunum1 > S926Wunum2).
'wu_greater'(S928Wu1, S929Wu2) -> (maps:get(number, S928Wu1) > maps:get(number, S929Wu2)).
'wu_equal'(S931Wu1, S932Wu2) -> (maps:get(number, S931Wu1) == maps:get(number, S932Wu2)).
    big_greater(Big1, Big2) ->
        Big1 > Big2.
  
    sort(Cards) ->
        lists:sort(
            fun(Card1, Card2) ->
                case {Card1, Card2} of
                    {{wu, #{number := Wunum1, design := B1}},
                     {wu, #{number := Wunum2, design := B2}}} ->
                        case Wunum1 =:= Wunum2 of
                            true  -> B1 =< B2;
                            false -> Wunum1 < Wunum2
                        end;
                    {{wen, Wen1}, {wen, Wen2}} ->
                        Wen1 =< Wen2;
                    {{wen, _}, {wu, _}} ->
                        true;
                    {{wu, _}, {wen, _}} ->
                        false
                end
            end,
            Cards).
  
'big_to_wen_and_wu'(S936Big) -> case S936Big of 'big_a' -> {8, 5}; 'big_b' -> {9, 7}; 'big_c' -> {10, 8}; 'big_d' -> {11, 9} end.
'wen_and_wu_to_big'(S938Wen, S939Wunum) -> case {S938Wen, S939Wunum} of {8, 5} -> {'ok', 'big_a'}; {9, 7} -> {'ok', 'big_b'}; {10, 8} -> {'ok', 'big_c'}; {11, 9} -> {'ok', 'big_d'}; _ -> 'error' end.
