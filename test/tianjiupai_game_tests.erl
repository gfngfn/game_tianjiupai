-module(tianjiupai_game_tests).

-include_lib("eunit/include/eunit.hrl").

quad_map_test_() ->
    [
     {"quad_map",
      fun() ->
          F = fun(X) -> X + 1 end,
          InputQuad = {4, 4, 2, 3},
          OutputQuad = tianjiupai_game:quad_map(F, InputQuad),
          ?assertEqual({5, 5, 3, 4}, OutputQuad)
      end}
    ].

all_cards_test_() ->
    [
     {"all_cards",
      fun() ->
          AllCards = tianjiupai_game:all_cards(),
          ?assertEqual(32, erlang:length(AllCards))
      end}
    ].

shuffle_test_() ->
    [
     {"every hand consists of eight cards",
      fun() ->
          %% Strictly speaking, we should mock `rand'.
          {H0, H1, H2, H3} = tianjiupai_game:shuffle(),
          ?assertEqual(8, erlang:length(H0)),
          ?assertEqual(8, erlang:length(H1)),
          ?assertEqual(8, erlang:length(H2)),
          ?assertEqual(8, erlang:length(H3))
      end}
    ].
