-module('Tianjiupai.Quad').
-export(['seat_equal'/2, 'advance_seat'/2, 'decompose'/1, 'make'/1, 'to_list'/1, 'access'/2, 'update'/3, 'map'/2, 'find'/2]).
    seat_equal(S1, S2) ->
        S1 =:= S2.
  
'advance_seat'(S945S, S946N) -> case (S946N < 0) of true -> 'Tianjiupai.Quad':'advance_seat'(S945S, (S946N + 4)); false -> case (S946N == 0) of true -> S945S; false -> begin S947S = case S945S of 'seat_a' -> 'seat_b'; 'seat_b' -> 'seat_c'; 'seat_c' -> 'seat_d'; 'seat_d' -> 'seat_a' end, 'Tianjiupai.Quad':'advance_seat'(S947S, (S946N - 1)) end end end.
'decompose'(S948Quad) -> {maps:get(east, S948Quad), maps:get(south, S948Quad), maps:get(west, S948Quad), maps:get(north, S948Quad)}.
'make'(S950X) -> begin {S951X0, S952X1, S953X2, S954X3} = S950X, #{east => S951X0, north => S954X3, south => S952X1, west => S953X2} end.
'to_list'(S956Quad) -> begin {S957X0, S958X1, S959X2, S960X3} = 'Tianjiupai.Quad':'decompose'(S956Quad), [S957X0 | [S958X1 | [S959X2 | [S960X3 | []]]]] end.
'access'(S962Seat, S963Quad) -> begin {S964X0, S965X1, S966X2, S967X3} = 'Tianjiupai.Quad':'decompose'(S963Quad), case S962Seat of 'seat_a' -> S964X0; 'seat_b' -> S965X1; 'seat_c' -> S966X2; 'seat_d' -> S967X3 end end.
'update'(S969Seat, S970XNew, S971Quad) -> begin {S972X0, S973X1, S974X2, S975X3} = 'Tianjiupai.Quad':'decompose'(S971Quad), begin S976X = case S969Seat of 'seat_a' -> {S970XNew, S973X1, S974X2, S975X3}; 'seat_b' -> {S972X0, S970XNew, S974X2, S975X3}; 'seat_c' -> {S972X0, S973X1, S970XNew, S975X3}; 'seat_d' -> {S972X0, S973X1, S974X2, S970XNew} end, 'Tianjiupai.Quad':'make'(S976X) end end.
'map'(S978F, S979Quad) -> begin {S980X0, S981X1, S982X2, S983X3} = 'Tianjiupai.Quad':'decompose'(S979Quad), 'Tianjiupai.Quad':'make'({S978F(S980X0), S978F(S981X1), S978F(S982X2), S978F(S983X3)}) end.
'find'(S985F, S986Quad) -> begin {S987X0, S988X1, S989X2, S990X3} = 'Tianjiupai.Quad':'decompose'(S986Quad), case {S985F(S987X0), S985F(S988X1), S985F(S989X2), S985F(S990X3)} of {true, _, _, _} -> {'ok', {'seat_a', S987X0}}; {_, true, _, _} -> {'ok', {'seat_b', S988X1}}; {_, _, true, _} -> {'ok', {'seat_c', S989X2}}; {_, _, _, true} -> {'ok', {'seat_d', S990X3}}; _ -> 'error' end end.
