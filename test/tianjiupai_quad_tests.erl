-module(tianjiupai_quad_tests).

-include_lib("eunit/include/eunit.hrl").

advance_seat_test_() ->
    [
     {"seat",
      fun() ->
          Got = tianjiupai_quad:advance_seat(Seat, N),
          ?assertEqual(Got, Expected)
      end}
    ||
        {Seat, N, Expected} <- [
            {3, 4, 3},
            {2, 3, 1},
            {2, -3, 3}
        ]
    ].

map_test_() ->
    [
     {"map",
      fun() ->
          F = fun(X) -> X + 1 end,
          InputQuad = {4, 4, 2, 3},
          OutputQuad = tianjiupai_quad:map(F, InputQuad),
          ?assertEqual({5, 5, 3, 4}, OutputQuad)
      end}
    ].

update_test_() ->
    [
     {"update",
      fun() ->
          Got = tianjiupai_quad:update(Seat, V, Input),
          ?assertEqual(Expected, Got)
      end
     }
    ||
        {Seat, V, Input, Expected} <- [
            {0, a, {x, y, z, w}, {a, y, z, w}},
            {1, a, {x, y, z, w}, {x, a, z, w}},
            {2, a, {x, y, z, w}, {x, y, a, w}},
            {3, a, {x, y, z, w}, {x, y, z, a}}
        ]
    ].
