-module('Tianjiupai.Card').
-export(['wen_greater'/2, 'wen_equal'/2, 'wu_number_greater'/2, 'wu_greater'/2, 'wu_equal'/2, 'big_greater'/2, 'sort'/1, 'big_to_wen_and_wu'/1, 'wen_and_wu_to_big'/2]).
'wen_greater'(S985Wen1, S986Wen2) -> (S985Wen1 > S986Wen2).
'wen_equal'(S988Wen1, S989Wen2) -> (S988Wen1 == S989Wen2).
'wu_number_greater'(S991Wunum1, S992Wunum2) -> (S991Wunum1 > S992Wunum2).
'wu_greater'(S994Wu1, S995Wu2) -> (maps:get(number, S994Wu1) > maps:get(number, S995Wu2)).
'wu_equal'(S997Wu1, S998Wu2) -> (maps:get(number, S997Wu1) == maps:get(number, S998Wu2)).
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
  
'big_to_wen_and_wu'(S1002Big) -> case S1002Big of 'big_a' -> {8, 5}; 'big_b' -> {9, 7}; 'big_c' -> {10, 8}; 'big_d' -> {11, 9} end.
'wen_and_wu_to_big'(S1004Wen, S1005Wunum) -> case {S1004Wen, S1005Wunum} of {8, 5} -> {'ok', 'big_a'}; {9, 7} -> {'ok', 'big_b'}; {10, 8} -> {'ok', 'big_c'}; {11, 9} -> {'ok', 'big_d'}; _ -> 'error' end.
