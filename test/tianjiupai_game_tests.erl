-module(tianjiupai_game_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-define(MOCKED_HAND_QUAD, {
  [{wen, 7}, {wu, 3},  {wen, 4},  {wen, 4}, {wu, 6},  {wen, 8},  {wen, 5},  {wen, 3}],
  [{wen, 3}, {wen, 8}, {wen, 10}, {wu, 9},  {wen, 2}, {wu, 8},   {wu, 5},   {wen, 6}],
  [{wen, 9}, {wen, 2}, {wen, 10}, {wu, 7},  {wen, 7}, {wu, 8},   {wen, 11}, {wu, 7}],
  [{wen, 9}, {wen, 5}, {wen, 1},  {wen, 6}, {wu, 5},  {wen, 11}, {wu, 9},   {wen, 1}]
}).

%%====================================================================================================
%% Unit Tests
%%====================================================================================================
all_cards_test_() ->
    [
     {"The number of all cards is 32.",
      fun() ->
          AllCards = tianjiupai_game:all_cards(),
          ?assertEqual(32, erlang:length(AllCards))
      end}
    ].

shuffle_test_() ->
    [
     {"Every generated hand consists of eight cards.",
      fun() ->
          %% Strictly speaking, we should mock `rand'.
          {H0, H1, H2, H3} = tianjiupai_game:shuffle(),
          ?assertEqual(8, erlang:length(H0)),
          ?assertEqual(8, erlang:length(H1)),
          ?assertEqual(8, erlang:length(H2)),
          ?assertEqual(8, erlang:length(H3))
      end}
    ].

make_starting_table_test_() ->
    First = fun(X) -> {X, []} end,
    [
     {"Makes a starting table",
      fun() ->
          Got = tianjiupai_game:make_starting_table(SubmittedCards),
          ?assertEqual(Expected, Got)
      end}
    ||
        {SubmittedCards, Expected} <- [
            {[{wen, 11}],
               {ok, {single_wen, First(11)}}},

            {[{wu, 8}],
               {ok, {single_wu, First(8)}}},

            {[{wen, 4}, {wen, 4}],
               {ok, {double_wen, First(4)}}},

            {[{wu, 7}, {wu, 7}],
               {ok, {double_wu, First(7)}}},

            {[{wen, 1}, {wen, 1}],
               {ok, {wenzun, First(minor)}}},

            {[{wu, 3}, {wu, 6}],
               {ok, {wuzun, First(ok)}}},

            {[{wen, 11}, {wu, 9}],
               {ok, {double_both, First(big4)}}},
            {[{wen, 10}, {wu, 8}],
               {ok, {double_both, First(big3)}}},
            {[{wen, 9}, {wu, 7}],
               {ok, {double_both, First(big2)}}},
            {[{wen, 8}, {wu, 5}],
               {ok, {double_both, First(big1)}}},

            {[{wu, 9}, {wen, 11}],
               {ok, {double_both, First(big4)}}},
            {[{wu, 8}, {wen, 10}],
               {ok, {double_both, First(big3)}}},
            {[{wu, 7}, {wen, 9}],
               {ok, {double_both, First(big2)}}},
            {[{wu, 5}, {wen, 8}],
               {ok, {double_both, First(big1)}}},

            {[{wen, 11}, {wu, 9}, {wen, 11}],
               {ok, {triple_wen, First(big4)}}},
            {[{wu, 8}, {wen, 10}, {wen, 10}],
               {ok, {triple_wen, First(big3)}}},
            {[{wen, 9}, {wen, 9}, {wu, 7}],
               {ok, {triple_wen, First(big2)}}},
            {[{wen, 8}, {wu, 5}, {wen, 8}],
               {ok, {triple_wen, First(big1)}}},

            {[],
               error},
            {[{wen, 4}, {wen, 5}],
               error},
            {[{wen, 11}, {wu, 8}],
               error}
        ]
    ].
