-module('Tianjiupai.Quad').
-export(['seat_equal'/2, 'advance_seat'/2, 'decompose'/1, 'make'/1, 'to_list'/1, 'access'/2, 'update'/3, 'map'/2, 'find'/2]).
    seat_equal(S1, S2) ->
        S1 =:= S2.
  
'advance_seat'(S944S, S945N) -> case (S945N < 0) of true -> 'Tianjiupai.Quad':'advance_seat'(S944S, (S945N + 4)); false -> case (S945N == 0) of true -> S944S; false -> begin S946S = case S944S of 'seat_a' -> 'seat_b'; 'seat_b' -> 'seat_c'; 'seat_c' -> 'seat_d'; 'seat_d' -> 'seat_a' end, 'Tianjiupai.Quad':'advance_seat'(S946S, (S945N - 1)) end end end.
'decompose'(S947Quad) -> {maps:get(east, S947Quad), maps:get(south, S947Quad), maps:get(west, S947Quad), maps:get(north, S947Quad)}.
'make'(S949X) -> begin {S950X0, S951X1, S952X2, S953X3} = S949X, #{east => S950X0, north => S953X3, south => S951X1, west => S952X2} end.
'to_list'(S955Quad) -> begin {S956X0, S957X1, S958X2, S959X3} = 'Tianjiupai.Quad':'decompose'(S955Quad), [S956X0 | [S957X1 | [S958X2 | [S959X3 | []]]]] end.
'access'(S961Seat, S962Quad) -> begin {S963X0, S964X1, S965X2, S966X3} = 'Tianjiupai.Quad':'decompose'(S962Quad), case S961Seat of 'seat_a' -> S963X0; 'seat_b' -> S964X1; 'seat_c' -> S965X2; 'seat_d' -> S966X3 end end.
'update'(S968Seat, S969XNew, S970Quad) -> begin {S971X0, S972X1, S973X2, S974X3} = 'Tianjiupai.Quad':'decompose'(S970Quad), begin S975X = case S968Seat of 'seat_a' -> {S969XNew, S972X1, S973X2, S974X3}; 'seat_b' -> {S971X0, S969XNew, S973X2, S974X3}; 'seat_c' -> {S971X0, S972X1, S969XNew, S974X3}; 'seat_d' -> {S971X0, S972X1, S973X2, S969XNew} end, 'Tianjiupai.Quad':'make'(S975X) end end.
'map'(S977F, S978Quad) -> begin {S979X0, S980X1, S981X2, S982X3} = 'Tianjiupai.Quad':'decompose'(S978Quad), 'Tianjiupai.Quad':'make'({S977F(S979X0), S977F(S980X1), S977F(S981X2), S977F(S982X3)}) end.
'find'(S984F, S985Quad) -> begin {S986X0, S987X1, S988X2, S989X3} = 'Tianjiupai.Quad':'decompose'(S985Quad), case {S984F(S986X0), S984F(S987X1), S984F(S988X2), S984F(S989X3)} of {true, _, _, _} -> {'ok', {'seat_a', S986X0}}; {_, true, _, _} -> {'ok', {'seat_b', S987X1}}; {_, _, true, _} -> {'ok', {'seat_c', S988X2}}; {_, _, _, true} -> {'ok', {'seat_d', S989X3}}; _ -> 'error' end end.
