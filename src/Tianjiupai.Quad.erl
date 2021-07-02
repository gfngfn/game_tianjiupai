-module('Tianjiupai.Quad').
-export(['seat_equal'/2, 'advance_seat'/2, 'to_list'/1, 'access'/2, 'update'/3, 'map'/2, 'find'/2]).

    seat_equal(S1, S2) ->
        S1 =:= S2.
  
'advance_seat'(S324S, S325N) -> case (S325N < 0) of true -> 'Tianjiupai.Quad':'advance_seat'(S324S, (S325N + 4)); false -> case (S325N == 0) of true -> S324S; false -> begin S326S = case S324S of 'seat0' -> 'seat1'; 'seat1' -> 'seat2'; 'seat2' -> 'seat3'; 'seat3' -> 'seat0' end, 'Tianjiupai.Quad':'advance_seat'(S326S, (S325N - 1)) end end end.
'to_list'(S327Quad) -> begin {S328X0, S329X1, S330X2, S331X3} = S327Quad, [S328X0 | [S329X1 | [S330X2 | [S331X3 | []]]]] end.
'access'(S333Seat, S334Quad) -> begin {S335X0, S336X1, S337X2, S338X3} = S334Quad, case S333Seat of 'seat0' -> S335X0; 'seat1' -> S336X1; 'seat2' -> S337X2; 'seat3' -> S338X3 end end.
'update'(S340Seat, S341XNew, S342Quad) -> begin {S343X0, S344X1, S345X2, S346X3} = S342Quad, case S340Seat of 'seat0' -> {S341XNew, S344X1, S345X2, S346X3}; 'seat1' -> {S343X0, S341XNew, S345X2, S346X3}; 'seat2' -> {S343X0, S344X1, S341XNew, S346X3}; 'seat3' -> {S343X0, S344X1, S345X2, S341XNew} end end.
'map'(S348F, S349Quad) -> begin {S350X0, S351X1, S352X2, S353X3} = S349Quad, {S348F(S350X0), S348F(S351X1), S348F(S352X2), S348F(S353X3)} end.
'find'(S355F, S356Quad) -> begin {S357X0, S358X1, S359X2, S360X3} = S356Quad, case {S355F(S357X0), S355F(S358X1), S355F(S359X2), S355F(S360X3)} of {true, _, _, _} -> {'ok', {'seat0', S357X0}}; {_, true, _, _} -> {'ok', {'seat1', S358X1}}; {_, _, true, _} -> {'ok', {'seat2', S359X2}}; {_, _, _, true} -> {'ok', {'seat3', S360X3}}; _ -> 'error' end end.
