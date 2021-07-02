-module('Tianjiupai.Card').
-export(['wen_greater'/2, 'wen_equal'/2, 'wu_number_greater'/2, 'wu_greater'/2, 'wu_equal'/2, 'big_greater'/2, 'sort'/1, 'big_to_wen_and_wu'/1, 'wen_and_wu_to_big'/2]).
'wen_greater'(S363Wen1, S364Wen2) -> (S363Wen1 > S364Wen2).
'wen_equal'(S366Wen1, S367Wen2) -> (S366Wen1 == S367Wen2).
'wu_number_greater'(S369Wunum1, S370Wunum2) -> (S369Wunum1 > S370Wunum2).
'wu_greater'(S372Wu1, S373Wu2) -> (maps:get(number, S372Wu1) > maps:get(number, S373Wu2)).
'wu_equal'(S375Wu1, S376Wu2) -> (maps:get(number, S375Wu1) == maps:get(number, S376Wu2)).

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
  
'big_to_wen_and_wu'(S380Big) -> case S380Big of 'big1' -> {8, 5}; 'big2' -> {9, 7}; 'big3' -> {10, 8}; 'big4' -> {11, 9} end.
'wen_and_wu_to_big'(S382Wen, S383Wunum) -> case {S382Wen, S383Wunum} of {8, 5} -> {'ok', 'big1'}; {9, 7} -> {'ok', 'big2'}; {10, 8} -> {'ok', 'big3'}; {11, 9} -> {'ok', 'big4'}; _ -> 'error' end.
