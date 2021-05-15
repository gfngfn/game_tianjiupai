-module(tianjiupai_quad_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TARGET_MODULE, 'Tianjiupai.Quad').

advance_seat_test_() ->
    [
     {"seat",
      fun() ->
          Got = ?TARGET_MODULE:advance_seat(Seat, N),
          ?assertEqual(Got, Expected)
      end}
    ||
        {Seat, N, Expected} <- [
            {seat3, 4, seat3},
            {seat2, 3, seat1},
            {seat2, -3, seat3}
        ]
    ].

map_test_() ->
    [
     {"map",
      fun() ->
          F = fun(X) -> X + 1 end,
          InputQuad = {4, 4, 2, 3},
          OutputQuad = ?TARGET_MODULE:map(F, InputQuad),
          ?assertEqual({5, 5, 3, 4}, OutputQuad)
      end}
    ].

update_test_() ->
    [
     {"update",
      fun() ->
          Got = ?TARGET_MODULE:update(Seat, V, Input),
          ?assertEqual(Expected, Got)
      end
     }
    ||
        {Seat, V, Input, Expected} <- [
            {seat0, a, {x, y, z, w}, {a, y, z, w}},
            {seat1, a, {x, y, z, w}, {x, a, z, w}},
            {seat2, a, {x, y, z, w}, {x, y, a, w}},
            {seat3, a, {x, y, z, w}, {x, y, z, a}}
        ]
    ].
