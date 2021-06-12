-module(tianjiupai_game_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-define(TARGET_MODULE, 'Tianjiupai.Inning').
-define(CARD_MODULE, 'Tianjiupai.Card').
-define(QUAD_MODULE, 'Tianjiupai.Quad').

-define(SEAT0, seat0).
-define(SEAT1, seat1).
-define(SEAT2, seat2).
-define(SEAT3, seat3).

-define(OK(_X_), {ok, _X_}).
-define(ERROR, error).

-define(MOCKED_HAND1, [{wen, 7}, {wu, 3}, {wen, 4}, {wen, 4}, {wu, 6}, {wen, 8}, {wen, 5}, {wen, 3}]).
-define(MOCKED_HAND2, [{wen, 3}, {wen, 8}, {wen, 10}, {wu, 9}, {wen, 2}, {wu, 8}, {wu, 5}, {wen, 6}]).
-define(MOCKED_HAND3, [{wen, 9}, {wen, 2}, {wen, 10}, {wu, 7}, {wen, 7}, {wu, 8}, {wen, 11}, {wu, 7}]).
-define(MOCKED_HAND4, [{wen, 9}, {wen, 5}, {wen, 1}, {wen, 6}, {wu, 5}, {wen, 11}, {wu, 9}, {wen, 1}]).

%% MOCKED_HAND11 := MOCKED_HAND1 - {wen, 7}
-define(MOCKED_HAND11, [{wu, 3}, {wen, 4}, {wen, 4}, {wu, 6}, {wen, 8}, {wen, 5}, {wen, 3}]).

%% MOCKED_HAND12 := MOCKED_HAND1 - {wu, 3}, {wu, 6}
-define(MOCKED_HAND12, [{wen, 7}, {wen, 4}, {wen, 4}, {wen, 8}, {wen, 5}, {wen, 3}]).

%% MOCKED_HAND21 := MOCKED_HAND2 - {wen, 8}
-define(MOCKED_HAND21, [{wen, 3}, {wen, 10}, {wu, 9}, {wen, 2}, {wu, 8}, {wu, 5}, {wen, 6}]).

%% MOCKED_HAND22 := MOCKED_HAND2 - {wen, 3}
-define(MOCKED_HAND22, [{wen, 8}, {wen, 10}, {wu, 9}, {wen, 2}, {wu, 8}, {wu, 5}, {wen, 6}]).

%% MOCKED_HAND31 := MOCKED_HAND3 - {wen, 2}
-define(MOCKED_HAND31, [{wen, 9}, {wen, 10}, {wu, 7}, {wen, 7}, {wu, 8}, {wen, 11}, {wu, 7}]).

%% MOCKED_HAND41 := MOCKED_HAND4 - {wen, 9}
-define(MOCKED_HAND41, [{wen, 5}, {wen, 1}, {wen, 6}, {wu, 5}, {wen, 11}, {wu, 9}, {wen, 1}]).

%% MOCKED_HAND42 := MOCKED_HAND4 - {wu, 5}
-define(MOCKED_HAND42, [{wen, 9}, {wen, 5}, {wen, 1}, {wen, 6}, {wen, 11}, {wu, 9}, {wen, 1}]).

-record(submit_test_case, {
    subtitle        :: string(),
    submitter_seat  :: ?QUAD_MODULE:seat(),
    submitter_cards :: [tianjiupai_game:card()],
    before          :: tianjiupai_game:inning_state(),
    expected        :: tianjiupai_game:submit_result()
}).

%%====================================================================================================
%% Unit Tests
%%====================================================================================================
zip_with_indices_test_() ->
    [
     {"zip_with_indices/1",
      fun() ->
          ?assertEqual([{0, a}, {1, b}, {2, c}], ?TARGET_MODULE:zip_with_indices([a, b, c]))
      end}
    ].

max_with_index_test_() ->
    [
     {"max_with_index/1",
      fun() ->
          ?assertEqual({2, d}, ?TARGET_MODULE:max_with_index(fun(X1, X2) -> X1 > X2 end, [a, c, d, b]))
      end}
    ].

update_table_success_test_() ->
    First = fun(X) -> {X, []} end,
    Exposed = fun(X, XOrCloseds) -> {X, XOrCloseds} end,
    [
     {"Succeeds in updating table states.",
      fun() ->
          Got = ?TARGET_MODULE:update_table(SubmittedCards, Table),
          ?assertEqual(?OK(Expected), Got)
      end}
    ||
        {SubmittedCards, Table, Expected} <- [
            %% Submits one card to `starting'.
            %% This pattern is tested by `make_starting_table_test_'.
            {[{wen, 5}],
                starting,
                {single_wen, First(5)}},

            %% Submits two cards to `wuzun'.
            {[{wen, 11}, {wen, 11}],
                {wuzun, Exposed(ok, [])},
                {wuzun, Exposed(ok, [closed])}},
            {[{wu, 9}, {wu, 9}],
                {wuzun, Exposed(ok, [closed])},
                {wuzun, Exposed(ok, [closed, closed])}},
            {[{wu, 9}, {wen, 10}],
                {wuzun, Exposed(ok, [closed])},
                {wuzun, Exposed(ok, [closed, closed])}},

            %% Submits two cards to `wenzun'.
            {[{wen, 2}, {wen, 2}],
                {wenzun, Exposed(minor, [])},
                {wenzun, Exposed(minor, [{open, major}])}},
            {[{wen, 2}, {wen, 2}],
                {wenzun, Exposed(minor, [closed])},
                {wenzun, Exposed(minor, [closed, {open, major}])}},
            {[{wen, 3}, {wen, 3}],
                {wenzun, Exposed(minor, [])},
                {wenzun, Exposed(minor, [closed])}},
            {[{wen, 3}, {wu, 5}],
                {wenzun, Exposed(minor, [])},
                {wenzun, Exposed(minor, [closed])}},

            %% Submits one wen to `single_wen'.
            {[{wen, 5}],
                {single_wen, Exposed(4, [])},
                {single_wen, Exposed(4, [{open, 5}])}},
            {[{wen, 5}],
                {single_wen, Exposed(5, [])},
                {single_wen, Exposed(5, [closed])}},
            {[{wen, 5}],
                {single_wen, Exposed(6, [])},
                {single_wen, Exposed(6, [closed])}},
            {[{wen, 5}],
                {single_wen, Exposed(3, [{open, 4}])},
                {single_wen, Exposed(3, [{open, 4}, {open, 5}])}},
            {[{wen, 5}],
                {single_wen, Exposed(3, [{open, 5}])},
                {single_wen, Exposed(3, [{open, 5}, closed])}},
            {[{wen, 5}],
                {single_wen, Exposed(3, [{open, 6}])},
                {single_wen, Exposed(3, [{open, 6}, closed])}},
            {[{wen, 5}],
                {single_wen, Exposed(3, [closed, {open, 6}])},
                {single_wen, Exposed(3, [closed, {open, 6}, closed])}},
            {[{wen, 5}],
                {single_wen, Exposed(3, [closed])},
                {single_wen, Exposed(3, [closed, {open, 5}])}},

            %% Submits one wu to `single_wen'.
            {[{wu, 5}],
                {single_wen, Exposed(6, [])},
                {single_wen, Exposed(6, [closed])}},
            {[{wu, 7}],
                {single_wen, Exposed(6, [])},
                {single_wen, Exposed(6, [closed])}},
            {[{wu, 7}],
                {single_wen, Exposed(6, [{open, 7}])},
                {single_wen, Exposed(6, [{open, 7}, closed])}},

            %% Submits one wu to `single_wu'.
            {[{wu, 8}],
                {single_wu, Exposed(6, [])},
                {single_wu, Exposed(6, [{open, 8}])}},
            {[{wu, 8}],
                {single_wu, Exposed(8, [])},
                {single_wu, Exposed(8, [closed])}},
            {[{wu, 8}],
                {single_wu, Exposed(9, [])},
                {single_wu, Exposed(9, [closed])}},
            {[{wu, 8}],
                {single_wu, Exposed(6, [{open, 7}])},
                {single_wu, Exposed(6, [{open, 7}, {open, 8}])}},
            {[{wu, 8}],
                {single_wu, Exposed(6, [{open, 8}])},
                {single_wu, Exposed(6, [{open, 8}, closed])}},
            {[{wu, 8}],
                {single_wu, Exposed(6, [{open, 9}])},
                {single_wu, Exposed(6, [{open, 9}, closed])}},
            {[{wu, 8}],
                {single_wu, Exposed(6, [closed, {open, 9}])},
                {single_wu, Exposed(6, [closed, {open, 9}, closed])}},
            {[{wu, 8}],
                {single_wu, Exposed(9, [closed])},
                {single_wu, Exposed(9, [closed, closed])}},

            %% Submits one wen to `single_wu'.
            {[{wen, 8}],
                {single_wu, Exposed(5, [])},
                {single_wu, Exposed(5, [closed])}},
            {[{wen, 8}],
                {single_wu, Exposed(9, [])},
                {single_wu, Exposed(9, [closed])}},
            {[{wen, 8}],
                {single_wu, Exposed(7, [{open, 9}])},
                {single_wu, Exposed(7, [{open, 9}, closed])}},

            %% Submits two same wens to `double_wen'.
            {[{wen, 5}, {wen, 5}],
                {double_wen, Exposed(4, [])},
                {double_wen, Exposed(4, [{open, 5}])}},
            {[{wen, 5}, {wen, 5}],
                {double_wen, Exposed(4, [{open, 6}])},
                {double_wen, Exposed(4, [{open, 6}, closed])}},
            {[{wen, 3}, {wen, 3}],
                {double_wen, Exposed(4, [])},
                {double_wen, Exposed(4, [closed])}},

            %% Submits an ineffective pair to `double_wen'.
            {[{wen, 7}, {wen, 2}],
                {double_wen, Exposed(6, [])},
                {double_wen, Exposed(6, [closed])}},
            {[{wen, 8}, {wen, 2}],
                {double_wen, Exposed(6, [{open, 7}])},
                {double_wen, Exposed(6, [{open, 7}, closed])}},
            {[{wu, 5}, {wen, 7}],
                {double_wen, Exposed(6, [])},
                {double_wen, Exposed(6, [closed])}},
            {[{wu, 5}, {wu, 7}],
                {double_wen, Exposed(6, [])},
                {double_wen, Exposed(6, [closed])}},

            %% Submits two same wus to `double_wu'.
            {[{wu, 8}, {wu, 8}],
                {double_wu, Exposed(5, [])},
                {double_wu, Exposed(5, [{open, 8}])}},
            {[{wu, 8}, {wu, 8}],
                {double_wu, Exposed(5, [{open, 7}])},
                {double_wu, Exposed(5, [{open, 7}, {open, 8}])}},
            {[{wu, 8}, {wu, 8}],
                {double_wu, Exposed(5, [{open, 9}])},
                {double_wu, Exposed(5, [{open, 9}, closed])}},

            %% Submits an ineffective pair to `double_wu'.
            {[{wu, 8}, {wen, 8}],
                {double_wu, Exposed(5, [])},
                {double_wu, Exposed(5, [closed])}},
            {[{wen, 8}, {wen, 8}],
                {double_wu, Exposed(5, [])},
                {double_wu, Exposed(5, [closed])}},
            {[{wen, 5}, {wen, 3}],
                {double_wu, Exposed(5, [{open, 7}])},
                {double_wu, Exposed(5, [{open, 7}, closed])}},

            %% Submits an effective triple to `triple_wen'.
            {[{wen, 11}, {wu, 9}, {wen, 11}],
                {triple_wen, Exposed(big3, [])},
                {triple_wen, Exposed(big3, [{open, big4}])}},
            {[{wu, 9}, {wen, 11}, {wen, 11}],
                {triple_wen, Exposed(big2, [{open, big3}])},
                {triple_wen, Exposed(big2, [{open, big3}, {open, big4}])}},
            {[{wen, 10}, {wu, 8}, {wen, 10}],
                {triple_wen, Exposed(big2, [])},
                {triple_wen, Exposed(big2, [{open, big3}])}},
            {[{wu, 5}, {wen, 8}, {wen, 8}],
                {triple_wen, Exposed(big2, [])},
                {triple_wen, Exposed(big2, [closed])}},

            %% Submits an ineffective triple to `triple_wen'.
            {[{wen, 11}, {wu, 9}, {wu, 9}],
                {triple_wen, Exposed(big3, [])},
                {triple_wen, Exposed(big3, [closed])}},
            {[{wen, 5}, {wen, 4}, {wen, 3}],
                {triple_wen, Exposed(big1, [])},
                {triple_wen, Exposed(big1, [closed])}},
            {[{wu, 5}, {wu, 5}, {wu, 7}],
                {triple_wen, Exposed(big1, [])},
                {triple_wen, Exposed(big1, [closed])}},

            %% Submits an effective triple to `triple_wu'.
            {[{wen, 11}, {wu, 9}, {wu, 9}],
                {triple_wu, Exposed(big3, [])},
                {triple_wu, Exposed(big3, [{open, big4}])}},
            {[{wu, 9}, {wen, 11}, {wu, 9}],
                {triple_wu, Exposed(big2, [{open, big3}])},
                {triple_wu, Exposed(big2, [{open, big3}, {open, big4}])}},
            {[{wen, 10}, {wu, 8}, {wu, 8}],
                {triple_wu, Exposed(big2, [])},
                {triple_wu, Exposed(big2, [{open, big3}])}},
            {[{wu, 5}, {wen, 8}, {wu, 5}],
                {triple_wu, Exposed(big2, [])},
                {triple_wu, Exposed(big2, [closed])}},

            %% Submits an ineffective triple to `triple_wu'.
            {[{wen, 11}, {wen, 11}, {wu, 9}],
                {triple_wu, Exposed(big3, [])},
                {triple_wu, Exposed(big3, [closed])}},
            {[{wen, 5}, {wen, 4}, {wen, 3}],
                {triple_wu, Exposed(big1, [])},
                {triple_wu, Exposed(big1, [closed])}},
            {[{wen, 11}, {wen, 11}, {wen, 10}],
                {triple_wu, Exposed(big3, [])},
                {triple_wu, Exposed(big3, [closed])}},

            %% Submits an effective quadruple to `quadruple'.
            {[{wen, 11}, {wu, 9}, {wu, 9}, {wen, 11}],
                {quadruple, Exposed(big3, [])},
                {quadruple, Exposed(big3, [{open, big4}])}},
            {[{wen, 11}, {wu, 9}, {wu, 9}, {wen, 11}],
                {quadruple, Exposed(big1, [{open, big2}])},
                {quadruple, Exposed(big1, [{open, big2}, {open, big4}])}},
            {[{wen, 9}, {wu, 7}, {wu, 7}, {wen, 9}],
                {quadruple, Exposed(big1, [{open, big3}])},
                {quadruple, Exposed(big1, [{open, big3}, closed])}},

            %% Submits an ineffective quadruple to `quadruple'.
            {[{wen, 10}, {wu, 9}, {wu, 9}, {wen, 11}],
                {quadruple, Exposed(big1, [])},
                {quadruple, Exposed(big1, [closed])}},
            {[{wen, 4}, {wu, 9}, {wu, 9}, {wen, 4}],
                {quadruple, Exposed(big1, [])},
                {quadruple, Exposed(big1, [closed])}}
        ]
    ].

get_winner_test_() ->
    Exposed = fun(X, Xs) -> {X, Xs} end,
    [
     {"Judge the winner.",
      fun() ->
          Got = ?TARGET_MODULE:get_winner(Table),
          ?assertEqual(Expected, Got)
      end}
    ||
      {Table, Expected} <- [
          %% Results of `wuzun'.
          {{wuzun, Exposed(ok, [closed, closed, closed])},
              {0, [{wu, 3}, {wu, 6}]}},

          %% Results of `wenzun'.
          {{wenzun, Exposed(minor, [closed, closed, closed])},
              {0, [{wen, 1}, {wen, 1}]}},
          {{wenzun, Exposed(minor, [closed, {open, major}, closed])},
              {2, [{wen, 2}, {wen, 2}]}},

          %% Results of `single_wen'.
          {{single_wen, Exposed(10, [closed, closed, closed])},
              {0, [{wen, 10}]}},
          {{single_wen, Exposed(6, [{open, 8}, {open, 11}, closed])},
              {2, [{wen, 11}]}},
          {{single_wen, Exposed(6, [{open, 8}, closed, {open, 11}])},
              {3, [{wen, 11}]}},
          {{single_wen, Exposed(8, [closed, closed, {open, 10}])},
              {3, [{wen, 10}]}},

          %% Results of `single_wu'.
          {{single_wu, Exposed(8, [closed, closed, closed])},
              {0, [{wu, 8}]}},
          {{single_wu, Exposed(3, [{open, 7}, {open, 9}, closed])},
              {2, [{wu, 9}]}},

          %% Results of `double_wen'.
          {{double_wen, Exposed(4, [closed, closed, closed])},
              {0, [{wen, 4}, {wen, 4}]}},
          {{double_wen, Exposed(6, [{open, 8}, {open, 11}, closed])},
              {2, [{wen, 11}, {wen, 11}]}},
          {{double_wen, Exposed(6, [{open, 8}, closed, {open, 11}])},
              {3, [{wen, 11}, {wen, 11}]}},
          {{double_wen, Exposed(8, [closed, closed, {open, 10}])},
              {3, [{wen, 10}, {wen, 10}]}},

          %% Results of `double_wu'.
          {{double_wu, Exposed(8, [closed, closed, closed])},
              {0, [{wu, 8}, {wu, 8}]}},
          {{double_wu, Exposed(3, [{open, 7}, {open, 9}, closed])},
              {2, [{wu, 9}, {wu, 9}]}},

          %% Results of `triple_wen'.
          {{triple_wen, Exposed(big2, [closed, closed, closed])},
              {0, [{wen, 9}, {wen, 9}, {wu, 7}]}},
          {{triple_wen, Exposed(big1, [{open, big2}, {open, big4}, closed])},
              {2, [{wen, 11}, {wen, 11}, {wu, 9}]}},
          {{triple_wen, Exposed(big1, [{open, big2}, closed, {open, big4}])},
              {3, [{wen, 11}, {wen, 11}, {wu, 9}]}},
          {{triple_wen, Exposed(big2, [closed, closed, {open, big3}])},
              {3, [{wen, 10}, {wen, 10}, {wu, 8}]}},

          %% Results of `triple_wu'.
          {{triple_wu, Exposed(big2, [closed, closed, closed])},
              {0, [{wen, 9}, {wu, 7}, {wu, 7}]}},
          {{triple_wu, Exposed(big1, [{open, big2}, {open, big4}, closed])},
              {2, [{wen, 11}, {wu, 9}, {wu, 9}]}},
          {{triple_wu, Exposed(big1, [{open, big2}, closed, {open, big4}])},
              {3, [{wen, 11}, {wu, 9}, {wu, 9}]}},
          {{triple_wu, Exposed(big2, [closed, closed, {open, big3}])},
              {3, [{wen, 10}, {wu, 8}, {wu, 8}]}},

          %% Results of `quadruple'.
          {{quadruple, Exposed(big1, [closed, closed, closed])},
              {0, [{wen, 8}, {wen, 8}, {wu, 5}, {wu, 5}]}},
          {{quadruple, Exposed(big1, [{open, big2}, {open, big4}, closed])},
              {2, [{wen, 11}, {wen, 11}, {wu, 9}, {wu, 9}]}},
          {{quadruple, Exposed(big1, [{open, big2}, closed, {open, big4}])},
              {3, [{wen, 11}, {wen, 11}, {wu, 9}, {wu, 9}]}},
          {{quadruple, Exposed(big2, [closed, closed, {open, big3}])},
              {3, [{wen, 10}, {wen, 10}, {wu, 8}, {wu, 8}]}}
      ]
    ].

submit_success_test_() ->
    Exposed = fun(X, XOrCloseds) -> {X, XOrCloseds} end,
    [
     {"submit (" ++ Subtitle ++ ")",
      fun() ->
          ?OK({Got, _IsFront}) = ?TARGET_MODULE:submit(SubmitterSeat, SubmittedCards, InningState),
          ?assertEqual(sort_hands_of_result(Expected), sort_hands_of_result(Got))
      end}
    ||
      #submit_test_case{
          subtitle        = Subtitle,
          before          = InningState,
          submitter_seat  = SubmitterSeat,
          submitter_cards = SubmittedCards,
          expected        = Expected
      } <- [
          #submit_test_case{
              subtitle = "first submission by Seat 0",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND1, []},
                      player1 => {?MOCKED_HAND2, []},
                      player2 => {?MOCKED_HAND3, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => starting
                  }),
              submitter_seat = ?SEAT0,
              submitter_cards = [{wen, 7}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []}, % MOCKED_HAND11 == MOCKED_HAND1 - {wen, 7}
                      player1 => {?MOCKED_HAND2, []},
                      player2 => {?MOCKED_HAND3, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [])}
                  })}
          },
          #submit_test_case{
              subtitle = "second submission by Seat 1 (superior)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND2, []},
                      player2 => {?MOCKED_HAND3, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [])}
                  }),
              submitter_seat = ?SEAT1,
              submitter_cards = [{wen, 8}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND21, []}, % MOCKED_HAND21 == MOCKED_HAND1 - {wen, 8}
                      player2 => {?MOCKED_HAND3, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [{open, 8}])}
                  })}
          },
          #submit_test_case{
              subtitle = "second submission by Seat 1 (inferior)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND2, []},
                      player2 => {?MOCKED_HAND3, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [])}
                  }),
              submitter_seat = ?SEAT1,
              submitter_cards = [{wen, 3}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND22, []}, % MOCKED_HAND22 == MOCKED_HAND1 - {wen, 3}
                      player2 => {?MOCKED_HAND3, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [closed])}
                  })}
          },
          #submit_test_case{
              subtitle = "fourth submission by Seat 3 (and Seat 3 wins the trick)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND21, []},
                      player2 => {?MOCKED_HAND31, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [{open, 8}, closed])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [{wen, 9}],
              expected =
                  {wins_trick, ?SEAT3,
                      {single_wen, Exposed(7, [{open, 8}, closed, {open, 9}])},
                      inning_state(#{
                          starts_at => ?SEAT3,
                          player0 => {?MOCKED_HAND11, []},
                          player1 => {?MOCKED_HAND21, []},
                          player2 => {?MOCKED_HAND31, []},
                          player3 => {?MOCKED_HAND41, [{wen, 9}]}, % MOCKED_HAND41 == MOCKED_HAND4 - {wen, 9}
                          table => starting
                      })}
          },
          #submit_test_case{
              subtitle = "fourth submission by Seat 3 (and Seat 1 wins the trick)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND21, []},
                      player2 => {?MOCKED_HAND31, []},
                      player3 => {?MOCKED_HAND4, []},
                      table => {single_wen, Exposed(7, [{open, 8}, closed])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [{wu, 5}],
              expected =
                  {wins_trick, ?SEAT1,
                      {single_wen, Exposed(7, [{open, 8}, closed, closed])},
                      inning_state(#{
                          starts_at => ?SEAT1,
                          player0 => {?MOCKED_HAND11, []},
                          player1 => {?MOCKED_HAND21, [{wen, 8}]},
                          player2 => {?MOCKED_HAND31, []},
                          player3 => {?MOCKED_HAND42, []}, % MOCKED_HAND42 == MOCKED_HAND4 - {wu, 5}
                          table => starting
                      })}
          },
          #submit_test_case{
              subtitle = "first submission by Seat 3",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {?MOCKED_HAND2, []},
                      player1 => {?MOCKED_HAND3, []},
                      player2 => {?MOCKED_HAND4, []},
                      player3 => {?MOCKED_HAND1, []},
                      table => starting
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [{wen, 7}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {?MOCKED_HAND2, []},
                      player1 => {?MOCKED_HAND3, []},
                      player2 => {?MOCKED_HAND4, []},
                      player3 => {?MOCKED_HAND11, []}, % MOCKED_HAND11 == MOCKED_HAND1 - {wen, 7}
                      table => {single_wen, Exposed(7, [])}
                  })}
          },
          #submit_test_case{
              subtitle = "second submission by Seat 0 (superior)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {?MOCKED_HAND2, []},
                      player1 => {?MOCKED_HAND3, []},
                      player2 => {?MOCKED_HAND4, []},
                      player3 => {?MOCKED_HAND11, []},
                      table => {single_wen, Exposed(7, [])}
                  }),
              submitter_seat = ?SEAT0,
              submitter_cards = [{wen, 8}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {?MOCKED_HAND21, []}, % MOCKED_HAND21 == MOCKED_HAND2 - {wen, 8}
                      player1 => {?MOCKED_HAND3, []},
                      player2 => {?MOCKED_HAND4, []},
                      player3 => {?MOCKED_HAND11, []},
                      table => {single_wen, Exposed(7, [{open, 8}])}
                  })}
          },
          #submit_test_case{
              subtitle = "wuzun submission by Seat 1",
              before =
                  inning_state(#{
                      starts_at => ?SEAT1,
                      player0 => {?MOCKED_HAND4, []},
                      player1 => {?MOCKED_HAND1, []},
                      player2 => {?MOCKED_HAND2, []},
                      player3 => {?MOCKED_HAND3, []},
                      table => starting
                  }),
              submitter_seat = ?SEAT1,
              submitter_cards = [{wu, 6}, {wu, 3}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT1,
                      player0 => {?MOCKED_HAND4, []},
                      player1 => {?MOCKED_HAND12, []}, % MOCKED_HAND12 == MOCKED_HAND1 - {wu, 3}, {wu, 6}
                      player2 => {?MOCKED_HAND2, []},
                      player3 => {?MOCKED_HAND3, []},
                      table => {wuzun, Exposed(ok, [])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, first submission by Seat 3",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[{wen, 11}], [{wu, 3}, {wu, 6}]},
                      player1 => {[{wu, 8}], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[{wu, 7}], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => starting
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [{wu, 7}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[{wen, 11}], [{wu, 3}, {wu, 6}]},
                      player1 => {[{wu, 8}], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => {single_wu, Exposed(7, [])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, second submission by Seat 0",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[{wen, 11}], [{wu, 3}, {wu, 6}]},
                      player1 => {[{wu, 8}], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => {single_wu, Exposed(7, [])}
                  }),
              submitter_seat = ?SEAT0,
              submitter_cards = [{wen, 11}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [{wu, 3}, {wu, 6}]},
                      player1 => {[{wu, 8}], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => {single_wu, Exposed(7, [closed])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, third submission by Seat 1 (not having rights to attend)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [{wu, 3}, {wu, 6}]},
                      player1 => {[{wu, 8}], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => {single_wu, Exposed(7, [closed])}
                  }),
              submitter_seat = ?SEAT1,
              submitter_cards = [{wu, 8}],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [{wu, 3}, {wu, 6}]},
                      player1 => {[], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => {single_wu, Exposed(7, [closed, closed])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, fourth submission by Seat 2 (and Seat 2 wins the inning)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [{wu, 3}, {wu, 6}]},
                      player1 => {[], []},
                      player2 => {[{wu, 9}], [{wen, 10}, {wu, 8}]},
                      player3 => {[], [{wen, 11}, {wen, 7}, {wen, 7}]},
                      table => {single_wu, Exposed(7, [closed, closed])}
                  }),
              submitter_seat = ?SEAT2,
              submitter_cards = [{wu, 9}],
              expected =
                  {wins_inning, ?SEAT2,
                      {single_wu, Exposed(7, [closed, closed, {open, 9}])},
                      {
                         [{wu, 3}, {wu, 6}],
                         [],
                         [{wen, 10}, {wu, 8}, {wu, 9}],
                         [{wen, 11}, {wen, 7}, {wen, 7}]
                      }}
          }
      ]
    ].

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
inning_state(#{
    starts_at := StartSeat,
    player0   := {Hand0, Gaineds0},
    player1   := {Hand1, Gaineds1},
    player2   := {Hand2, Gaineds2},
    player3   := {Hand3, Gaineds3},
    table     := TableState
}) ->
    PlayerQuad = {
        #{hand => Hand0, gained => Gaineds0},
        #{hand => Hand1, gained => Gaineds1},
        #{hand => Hand2, gained => Gaineds2},
        #{hand => Hand3, gained => Gaineds3}
    },
    #{
        starts_at => StartSeat,
        players   => PlayerQuad,
        table     => TableState
    }.

%% `fun(Inning.submit_result) -> Inning.submit_result'
sort_hands_of_result(Result) ->
    case Result of
        {continues, Next}                         -> {continues, sort_hands(Next)};
        {wins_trick, WinnerSeat, LastTable, Next} -> {wins_trick, WinnerSeat, LastTable, sort_hands(Next)};
        {wins_inning, _, _, _}                    -> Result
    end.

%% `fun(Inning.t) -> Inning.t'
sort_hands(InningState) ->
    #{
        starts_at := StartSeat,
        players   := PlayerQuad0,
        table     := TableState
    } = InningState,
    PlayerQuad1 =
        ?QUAD_MODULE:map(
            fun(Player) ->
                #{hand := Hand, gained := Gaineds} = Player,
                #{hand => ?CARD_MODULE:sort(Hand), gained => Gaineds}
            end,
            PlayerQuad0),
    #{
        starts_at => StartSeat,
        players   => PlayerQuad1,
        table     => TableState
    }.
