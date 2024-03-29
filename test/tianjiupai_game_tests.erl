-module(tianjiupai_game_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-define(TARGET_MODULE, 'Tianjiupai.Inning').
-define(CARD_MODULE, 'Tianjiupai.Card').
-define(QUAD_MODULE, 'Tianjiupai.Quad').

-define(SEAT0, seat_a).
-define(SEAT1, seat_b).
-define(SEAT2, seat_c).
-define(SEAT3, seat_d).

-define(OK(_X_), {ok, _X_}).
-define(ERROR, error).

-define(MOCKED_HAND1, [wen(7), wuT(3), wen(4), wen(4), wuT(6), wen(8), wen(5), wen(3)]).
-define(MOCKED_HAND2, [wen(3), wen(8), wen(10), wuT(9), wen(2), wuF(8), wuT(5), wen(6)]).
-define(MOCKED_HAND3, [wen(9), wen(2), wen(10), wuF(7), wen(7), wuT(8), wen(11), wuT(7)]).
-define(MOCKED_HAND4, [wen(9), wen(5), wen(1), wen(6), wuF(5), wen(11), wuF(9), wen(1)]).

%% MOCKED_HAND11 := MOCKED_HAND1 - wen(7)
-define(MOCKED_HAND11, [wuT(3), wen(4), wen(4), wuT(6), wen(8), wen(5), wen(3)]).

%% MOCKED_HAND12 := MOCKED_HAND1 - wuT(3), wuT(6)
-define(MOCKED_HAND12, [wen(7), wen(4), wen(4), wen(8), wen(5), wen(3)]).

%% MOCKED_HAND21 := MOCKED_HAND2 - wen(8)
-define(MOCKED_HAND21, [wen(3), wen(10), wuT(9), wen(2), wuF(8), wuT(5), wen(6)]).

%% MOCKED_HAND22 := MOCKED_HAND2 - wen(3)
-define(MOCKED_HAND22, [wen(8), wen(10), wuT(9), wen(2), wuF(8), wuT(5), wen(6)]).

%% MOCKED_HAND31 := MOCKED_HAND3 - wen(2)
-define(MOCKED_HAND31, [wen(9), wen(10), wuF(7), wen(7), wuT(8), wen(11), wuT(7)]).

%% MOCKED_HAND41 := MOCKED_HAND4 - wen(9)
-define(MOCKED_HAND41, [wen(5), wen(1), wen(6), wuF(5), wen(11), wuF(9), wen(1)]).

%% MOCKED_HAND42 := MOCKED_HAND4 - wu(5)
-define(MOCKED_HAND42, [wen(9), wen(5), wen(1), wen(6), wen(11), wuF(9), wen(1)]).

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
    Exposed = fun(X, XOrCloseds) -> #{first => X, subsequent => XOrCloseds} end,
    First = fun(X) -> Exposed(X, []) end,
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
            {[wen(5)],
                starting,
                {single_wen, First(5)}},

            %% Submits two cards to `wuzun'.
            {[wen(11), wen(11)],
                {wuzun, Exposed(ok, [])},
                {wuzun, Exposed(ok, [closed])}},
            {[wuF(9), wuT(9)],
                {wuzun, Exposed(ok, [closed])},
                {wuzun, Exposed(ok, [closed, closed])}},
            {[wuF(9), wen(10)],
                {wuzun, Exposed(ok, [closed])},
                {wuzun, Exposed(ok, [closed, closed])}},

            %% Submits two cards to `wenzun'.
            {[wen(2), wen(2)],
                {wenzun, Exposed(wenzun_minor, [])},
                {wenzun, Exposed(wenzun_minor, [{open, wenzun_major}])}},
            {[wen(2), wen(2)],
                {wenzun, Exposed(wenzun_minor, [closed])},
                {wenzun, Exposed(wenzun_minor, [closed, {open, wenzun_major}])}},
            {[wen(3), wen(3)],
                {wenzun, Exposed(wenzun_minor, [])},
                {wenzun, Exposed(wenzun_minor, [closed])}},
            {[wen(3), wuF(5)],
                {wenzun, Exposed(wenzun_minor, [])},
                {wenzun, Exposed(wenzun_minor, [closed])}},

            %% Submits one wen to `single_wen'.
            {[wen(5)],
                {single_wen, Exposed(4, [])},
                {single_wen, Exposed(4, [{open, 5}])}},
            {[wen(5)],
                {single_wen, Exposed(5, [])},
                {single_wen, Exposed(5, [closed])}},
            {[wen(5)],
                {single_wen, Exposed(6, [])},
                {single_wen, Exposed(6, [closed])}},
            {[wen(5)],
                {single_wen, Exposed(3, [{open, 4}])},
                {single_wen, Exposed(3, [{open, 4}, {open, 5}])}},
            {[wen(5)],
                {single_wen, Exposed(3, [{open, 5}])},
                {single_wen, Exposed(3, [{open, 5}, closed])}},
            {[wen(5)],
                {single_wen, Exposed(3, [{open, 6}])},
                {single_wen, Exposed(3, [{open, 6}, closed])}},
            {[wen(5)],
                {single_wen, Exposed(3, [closed, {open, 6}])},
                {single_wen, Exposed(3, [closed, {open, 6}, closed])}},
            {[wen(5)],
                {single_wen, Exposed(3, [closed])},
                {single_wen, Exposed(3, [closed, {open, 5}])}},

            %% Submits one wu to `single_wen'.
            {[wuT(5)],
                {single_wen, Exposed(6, [])},
                {single_wen, Exposed(6, [closed])}},
            {[wuT(7)],
                {single_wen, Exposed(6, [])},
                {single_wen, Exposed(6, [closed])}},
            {[wuF(7)],
                {single_wen, Exposed(6, [{open, 7}])},
                {single_wen, Exposed(6, [{open, 7}, closed])}},

            %% Submits one wu to `single_wu'.
            {[wuT(8)],
                {single_wu, Exposed(wunumT(6), [])},
                {single_wu, Exposed(wunumT(6), [{open, wunumT(8)}])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumF(8), [])},
                {single_wu, Exposed(wunumF(8), [closed])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumF(9), [])},
                {single_wu, Exposed(wunumF(9), [closed])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumT(6), [{open, wunumF(7)}])},
                {single_wu, Exposed(wunumT(6), [{open, wunumF(7)}, {open, wunumT(8)}])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumT(6), [{open, wunumF(8)}])},
                {single_wu, Exposed(wunumT(6), [{open, wunumF(8)}, closed])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumT(6), [{open, wunumF(9)}])},
                {single_wu, Exposed(wunumT(6), [{open, wunumF(9)}, closed])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumT(6), [closed, {open, wunumF(9)}])},
                {single_wu, Exposed(wunumT(6), [closed, {open, wunumF(9)}, closed])}},
            {[wuT(8)],
                {single_wu, Exposed(wunumF(9), [closed])},
                {single_wu, Exposed(wunumF(9), [closed, closed])}},

            %% Submits one wen to `single_wu'.
            {[wen(8)],
                {single_wu, Exposed(wunumT(5), [])},
                {single_wu, Exposed(wunumT(5), [closed])}},
            {[wen(8)],
                {single_wu, Exposed(wunumT(9), [])},
                {single_wu, Exposed(wunumT(9), [closed])}},
            {[wen(8)],
                {single_wu, Exposed(wunumT(7), [{open, wunumT(9)}])},
                {single_wu, Exposed(wunumT(7), [{open, wunumT(9)}, closed])}},

            %% Submits two same wens to `double_wen'.
            {[wen(5), wen(5)],
                {double_wen, Exposed(4, [])},
                {double_wen, Exposed(4, [{open, 5}])}},
            {[wen(5), wen(5)],
                {double_wen, Exposed(4, [{open, 6}])},
                {double_wen, Exposed(4, [{open, 6}, closed])}},
            {[wen(3), wen(3)],
                {double_wen, Exposed(4, [])},
                {double_wen, Exposed(4, [closed])}},

            %% Submits an ineffective pair to `double_wen'.
            {[wen(7), wen(2)],
                {double_wen, Exposed(6, [])},
                {double_wen, Exposed(6, [closed])}},
            {[wen(8), wen(2)],
                {double_wen, Exposed(6, [{open, 7}])},
                {double_wen, Exposed(6, [{open, 7}, closed])}},
            {[wuT(5), wen(7)],
                {double_wen, Exposed(6, [])},
                {double_wen, Exposed(6, [closed])}},
            {[wuT(5), wuF(7)],
                {double_wen, Exposed(6, [])},
                {double_wen, Exposed(6, [closed])}},

            %% Submits two same wus to `double_wu'.
            {[wuF(8), wuT(8)],
                {double_wu, Exposed(5, [])},
                {double_wu, Exposed(5, [{open, 8}])}},
            {[wuF(8), wuT(8)],
                {double_wu, Exposed(5, [{open, 7}])},
                {double_wu, Exposed(5, [{open, 7}, {open, 8}])}},
            {[wuF(8), wuT(8)],
                {double_wu, Exposed(5, [{open, 9}])},
                {double_wu, Exposed(5, [{open, 9}, closed])}},

            %% Submits an ineffective pair to `double_wu'.
            {[wuT(8), wen(8)],
                {double_wu, Exposed(5, [])},
                {double_wu, Exposed(5, [closed])}},
            {[wen(8), wen(8)],
                {double_wu, Exposed(5, [])},
                {double_wu, Exposed(5, [closed])}},
            {[wen(5), wen(3)],
                {double_wu, Exposed(5, [{open, 7}])},
                {double_wu, Exposed(5, [{open, 7}, closed])}},

            %% Submits an effective triple to `triple_wen'.
            {[wen(11), wuT(9), wen(11)],
                {triple_wen, Exposed(bigF(big_c), [])},
                {triple_wen, Exposed(bigF(big_c), [{open, bigT(big_d)}])}},
            {[wuT(9), wen(11), wen(11)],
                {triple_wen, Exposed(bigF(big_b), [{open, bigT(big_c)}])},
                {triple_wen, Exposed(bigF(big_b), [{open, bigT(big_c)}, {open, bigT(big_d)}])}},
            {[wen(10), wuT(8), wen(10)],
                {triple_wen, Exposed(bigF(big_b), [])},
                {triple_wen, Exposed(bigF(big_b), [{open, bigT(big_c)}])}},
            {[wuT(5), wen(8), wen(8)],
                {triple_wen, Exposed(bigF(big_b), [])},
                {triple_wen, Exposed(bigF(big_b), [closed])}},

            %% Submits an ineffective triple to `triple_wen'.
            {[wen(11), wuT(9), wuF(9)],
                {triple_wen, Exposed(bigT(big_c), [])},
                {triple_wen, Exposed(bigT(big_c), [closed])}},
            {[wen(5), wen(4), wen(3)],
                {triple_wen, Exposed(bigT(big_a), [])},
                {triple_wen, Exposed(bigT(big_a), [closed])}},
            {[wuF(5), wuT(5), wuF(7)],
                {triple_wen, Exposed(bigT(big_a), [])},
                {triple_wen, Exposed(bigT(big_a), [closed])}},

            %% Submits an effective triple to `triple_wu'.
            {[wen(11), wuT(9), wuF(9)],
                {triple_wu, Exposed(big_c, [])},
                {triple_wu, Exposed(big_c, [{open, big_d}])}},
            {[wuT(9), wen(11), wuF(9)],
                {triple_wu, Exposed(big_b, [{open, big_c}])},
                {triple_wu, Exposed(big_b, [{open, big_c}, {open, big_d}])}},
            {[wen(10), wuT(8), wuF(8)],
                {triple_wu, Exposed(big_b, [])},
                {triple_wu, Exposed(big_b, [{open, big_c}])}},
            {[wuT(5), wen(8), wuF(5)],
                {triple_wu, Exposed(big_b, [])},
                {triple_wu, Exposed(big_b, [closed])}},

            %% Submits an ineffective triple to `triple_wu'.
            {[wen(11), wen(11), wuT(9)],
                {triple_wu, Exposed(big_c, [])},
                {triple_wu, Exposed(big_c, [closed])}},
            {[wen(5), wen(4), wen(3)],
                {triple_wu, Exposed(big_a, [])},
                {triple_wu, Exposed(big_a, [closed])}},
            {[wen(11), wen(11), wen(10)],
                {triple_wu, Exposed(big_c, [])},
                {triple_wu, Exposed(big_c, [closed])}},

            %% Submits an effective quadruple to `quadruple'.
            {[wen(11), wuT(9), wuF(9), wen(11)],
                {quadruple, Exposed(big_c, [])},
                {quadruple, Exposed(big_c, [{open, big_d}])}},
            {[wen(11), wuT(9), wuF(9), wen(11)],
                {quadruple, Exposed(big_a, [{open, big_b}])},
                {quadruple, Exposed(big_a, [{open, big_b}, {open, big_d}])}},
            {[wen(9), wuF(7), wuT(7), wen(9)],
                {quadruple, Exposed(big_a, [{open, big_c}])},
                {quadruple, Exposed(big_a, [{open, big_c}, closed])}},

            %% Submits an ineffective quadruple to `quadruple'.
            {[wen(10), wuT(9), wuF(9), wen(11)],
                {quadruple, Exposed(big_a, [])},
                {quadruple, Exposed(big_a, [closed])}},
            {[wen(4), wuF(9), wuT(9), wen(4)],
                {quadruple, Exposed(big_a, [])},
                {quadruple, Exposed(big_a, [closed])}}
        ]
    ].

get_winner_test_() ->
    Exposed = fun(X, XOrCloseds) -> #{first => X, subsequent => XOrCloseds} end,
    [
     {"Judge the winner.",
      fun() ->
          Got = ?TARGET_MODULE:get_winner(Table),
          ?assertEqual(Expected, Got)
      end}
    ||
      {Table, Expected} <- [
          %% Results of `wuzun'.
          {{wuzun, Exposed(wuzun_unit, [closed, closed, closed])},
              {0, [wuT(3), wuT(6)], {ok, zhizun}}},

          %% Results of `wenzun'.
          {{wenzun, Exposed(wenzun_minor, [closed, closed, closed])},
              {0, [wen(1), wen(1)], {ok, zhizun}}},
          {{wenzun, Exposed(wenzun_minor, [closed, {open, wenzun_major}, closed])},
              {2, [wen(2), wen(2)], {ok, zhizun}}},

          %% Results of `single_wen'.
          {{single_wen, Exposed(10, [closed, closed, closed])},
              {0, [wen(10)], error}},
          {{single_wen, Exposed(6, [{open, 8}, {open, 11}, closed])},
              {2, [wen(11)], error}},
          {{single_wen, Exposed(6, [{open, 8}, closed, {open, 11}])},
              {3, [wen(11)], error}},
          {{single_wen, Exposed(8, [closed, closed, {open, 10}])},
              {3, [wen(10)], error}},

          %% Results of `single_wu'.
          {{single_wu, Exposed(wunumT(8), [closed, closed, closed])},
              {0, [wuT(8)], error}},
          {{single_wu, Exposed(wunumT(3), [{open, wunumF(7)}, {open, wunumF(9)}, closed])},
              {2, [wuF(9)], error}},

          %% Results of `double_wen'.
          {{double_wen, Exposed(4, [closed, closed, closed])},
              {0, [wen(4), wen(4)], error}},
          {{double_wen, Exposed(6, [{open, 8}, {open, 11}, closed])},
              {2, [wen(11), wen(11)], error}},
          {{double_wen, Exposed(6, [{open, 8}, closed, {open, 11}])},
              {3, [wen(11), wen(11)], error}},
          {{double_wen, Exposed(8, [closed, closed, {open, 10}])},
              {3, [wen(10), wen(10)], error}},

          %% Results of `double_wu'.
          {{double_wu, Exposed(8, [closed, closed, closed])},
              {0, [wuF(8), wuT(8)], error}},
          {{double_wu, Exposed(3, [{open, 7}, {open, 9}, closed])},
              {2, [wuF(9), wuT(9)], error}},

          %% Results of `triple_wen'.
          {{triple_wen, Exposed(bigT(big_b), [closed, closed, closed])},
              {0, [wen(9), wen(9), wuT(7)], error}},
          {{triple_wen, Exposed(bigF(big_a), [{open, bigF(big_b)}, {open, bigT(big_d)}, closed])},
              {2, [wen(11), wen(11), wuT(9)], error}},
          {{triple_wen, Exposed(bigF(big_a), [{open, bigF(big_b)}, closed, {open, bigT(big_d)}])},
              {3, [wen(11), wen(11), wuT(9)], error}},
          {{triple_wen, Exposed(bigT(big_b), [closed, closed, {open, bigF(big_c)}])},
              {3, [wen(10), wen(10), wuF(8)], error}},

          %% Results of `triple_wu'.
          {{triple_wu, Exposed(big_b, [closed, closed, closed])},
              {0, [wen(9), wuF(7), wuT(7)], error}},
          {{triple_wu, Exposed(big_a, [{open, big_b}, {open, big_d}, closed])},
              {2, [wen(11), wuF(9), wuT(9)], error}},
          {{triple_wu, Exposed(big_a, [{open, big_b}, closed, {open, big_d}])},
              {3, [wen(11), wuF(9), wuT(9)], error}},
          {{triple_wu, Exposed(big_b, [closed, closed, {open, big_c}])},
              {3, [wen(10), wuF(8), wuT(8)], error}},

          %% Results of `quadruple'.
          {{quadruple, Exposed(big_a, [closed, closed, closed])},
              {0, [wen(8), wen(8), wuF(5), wuT(5)], {ok, sidahe}}},
          {{quadruple, Exposed(big_a, [{open, big_b}, {open, big_d}, closed])},
              {2, [wen(11), wen(11), wuF(9), wuT(9)], {ok, sidahe}}},
          {{quadruple, Exposed(big_a, [{open, big_b}, closed, {open, big_d}])},
              {3, [wen(11), wen(11), wuF(9), wuT(9)], {ok, sidahe}}},
          {{quadruple, Exposed(big_b, [closed, closed, {open, big_c}])},
              {3, [wen(10), wen(10), wuF(8), wuT(8)], {ok, sidahe}}}
      ]
    ].

submit_success_test_() ->
    Exposed = fun(X, XOrCloseds) -> #{first => X, subsequent => XOrCloseds} end,
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
              submitter_cards = [wen(7)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []}, % MOCKED_HAND11 == MOCKED_HAND1 - wen(7)
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
              submitter_cards = [wen(8)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND21, []}, % MOCKED_HAND21 == MOCKED_HAND1 - wen(8)
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
              submitter_cards = [wen(3)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {?MOCKED_HAND11, []},
                      player1 => {?MOCKED_HAND22, []}, % MOCKED_HAND22 == MOCKED_HAND1 - wen(3)
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
              submitter_cards = [wen(9)],
              expected =
                  {wins_trick, ?SEAT3,
                      {single_wen, Exposed(7, [{open, 8}, closed, {open, 9}])},
                      ?MOCKED_HAND41,
                      error,
                      inning_state(#{
                          starts_at => ?SEAT3,
                          player0 => {?MOCKED_HAND11, []},
                          player1 => {?MOCKED_HAND21, []},
                          player2 => {?MOCKED_HAND31, []},
                          player3 => {?MOCKED_HAND41, [wen(9)]}, % MOCKED_HAND41 == MOCKED_HAND4 - wen(9)
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
              submitter_cards = [wuF(5)],
              expected =
                  {wins_trick, ?SEAT1,
                      {single_wen, Exposed(7, [{open, 8}, closed, closed])},
                      ?MOCKED_HAND42,
                      error,
                      inning_state(#{
                          starts_at => ?SEAT1,
                          player0 => {?MOCKED_HAND11, []},
                          player1 => {?MOCKED_HAND21, [wen(8)]},
                          player2 => {?MOCKED_HAND31, []},
                          player3 => {?MOCKED_HAND42, []}, % MOCKED_HAND42 == MOCKED_HAND4 - wu(5)
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
              submitter_cards = [wen(7)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {?MOCKED_HAND2, []},
                      player1 => {?MOCKED_HAND3, []},
                      player2 => {?MOCKED_HAND4, []},
                      player3 => {?MOCKED_HAND11, []}, % MOCKED_HAND11 == MOCKED_HAND1 - wen(7)
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
              submitter_cards = [wen(8)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {?MOCKED_HAND21, []}, % MOCKED_HAND21 == MOCKED_HAND2 - wen(8)
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
              submitter_cards = [wuT(6), wuT(3)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT1,
                      player0 => {?MOCKED_HAND4, []},
                      player1 => {?MOCKED_HAND12, []}, % MOCKED_HAND12 == MOCKED_HAND1 - wuT(3), wuT(6)
                      player2 => {?MOCKED_HAND2, []},
                      player3 => {?MOCKED_HAND3, []},
                      table => {wuzun, Exposed(wuzun_unit, [])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, first submission by Seat 3",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[wen(11)], [wuT(3), wuT(6)]},
                      player1 => {[wuT(8)], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[wuF(7)], [wen(11), wen(7), wen(7)]},
                      table => starting
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wuF(7)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[wen(11)], [wuT(3), wuT(6)]},
                      player1 => {[wuT(8)], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[], [wen(11), wen(7), wen(7)]},
                      table => {single_wu, Exposed(wunumF(7), [])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, second submission by Seat 0",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[wen(11)], [wuT(3), wuT(6)]},
                      player1 => {[wuT(8)], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[], [wen(11), wen(7), wen(7)]},
                      table => {single_wu, Exposed(wunumT(7), [])}
                  }),
              submitter_seat = ?SEAT0,
              submitter_cards = [wen(11)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [wuT(3), wuT(6)]},
                      player1 => {[wuT(8)], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[], [wen(11), wen(7), wen(7)]},
                      table => {single_wu, Exposed(wunumT(7), [closed])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, third submission by Seat 1 (not having rights to attend)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [wuT(3), wuT(6)]},
                      player1 => {[wuT(8)], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[], [wen(11), wen(7), wen(7)]},
                      table => {single_wu, Exposed(wunumT(7), [closed])}
                  }),
              submitter_seat = ?SEAT1,
              submitter_cards = [wuT(8)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [wuT(3), wuT(6)]},
                      player1 => {[], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[], [wen(11), wen(7), wen(7)]},
                      table => {single_wu, Exposed(wunumT(7), [closed, closed])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick, fourth submission by Seat 2 (and Seat 2 wins the inning)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [wuT(3), wuT(6)]},
                      player1 => {[], []},
                      player2 => {[wuT(9)], [wen(10), wuF(8)]},
                      player3 => {[], [wen(11), wen(7), wen(7)]},
                      table => {single_wu, Exposed(wunumT(7), [closed, closed])}
                  }),
              submitter_seat = ?SEAT2,
              submitter_cards = [wuT(9)],
              expected =
                  {wins_inning, ?SEAT2,
                      {single_wu, Exposed(wunumT(7), [closed, closed, {open, wunumT(9)}])},
                      #{
                         east  => [wuT(3), wuT(6)],
                         south => [],
                         west  => [wen(10), wuF(8), wuT(9)],
                         north => [wen(11), wen(7), wen(7)]
                      },
                      error}
          },
          #submit_test_case{
              subtitle = "last trick, fourth double submission by Seat 2 (and Seat 2 wins the inning)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT3,
                      player0 => {[], [wuT(3), wuT(6)]},
                      player1 => {[], [wen(10), wuF(8)]},
                      player2 => {[wuT(7), wuF(7)], []},
                      player3 => {[], [wen(11), wuF(9)]},
                      table => {double_wu, Exposed(5, [closed, closed])}
                  }),
              submitter_seat = ?SEAT2,
              submitter_cards = [wuT(7), wuF(7)],
              expected =
                  {wins_inning, ?SEAT2,
                      {double_wu, Exposed(5, [closed, closed, {open, 7}])},
                      #{
                         east  => [wuT(3), wuT(6)],
                         south => [wen(10), wuF(8)],
                         west  => [wuF(7), wuT(7)],
                         north => [wen(11), wuF(9)]
                      },
                      error}
          },
          #submit_test_case{
              subtitle = "inning ends with Wuzun",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wen(9)]},
                      player1 => {[], [wen(10), wuF(8)]},
                      player2 => {[], [wuT(5), wuF(5)]},
                      player3 => {[wen(11), wuT(9)], []},
                      table => {wuzun, Exposed(wuzun_unit, [closed, closed])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wen(11), wuT(9)],
              expected =
                  {wins_inning, ?SEAT0,
                      {wuzun, Exposed(wuzun_unit, [closed, closed, closed])},
                      #{
                          east  => [wen(11), wen(9), wuT(3), wuT(6)],
                          south => [wen(10), wuF(8)],
                          west  => [wuT(5), wuF(5)],
                          north => []
                      },
                      {ok, inning_end_with_zhizun}}
          },
          #submit_test_case{
              subtitle = "inning ends with Yaojie 1",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wen(9)]},
                      player1 => {[], [wen(10), wuF(8), wuT(8)]},
                      player2 => {[], [wuT(5), wuF(5)]},
                      player3 => {[wen(11)], []},
                      table => {single_wu, Exposed(wunumT(3), [closed, closed])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wen(11)],
              expected =
                  {wins_inning, ?SEAT0,
                      {single_wu, Exposed(wunumT(3), [closed, closed, closed])},
                      #{
                          east  => [wen(11), wen(9), wuT(3)],
                          south => [wen(10), wuF(8), wuT(8)],
                          west  => [wuT(5), wuF(5)],
                          north => []
                      },
                      {ok, inning_end_with_yaojie}}
          },
          #submit_test_case{
              subtitle = "inning ends with Yaojie 2",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wen(9)]},
                      player1 => {[], [wen(10), wuF(8), wuT(8)]},
                      player2 => {[], [wuT(5), wuF(5)]},
                      player3 => {[wen(11)], []},
                      table => {single_wu, Exposed(wunumT(3), [closed, {open, wunumT(6)}])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wen(11)],
              expected =
                  {wins_inning, ?SEAT2,
                      {single_wu, Exposed(wunumT(3), [closed, {open, wunumT(6)}, closed])},
                      #{
                          east  => [wen(11), wen(9)],
                          south => [wen(10), wuF(8), wuT(8)],
                          west  => [wuT(5), wuF(5), wuT(6)],
                          north => []
                      },
                      {ok, inning_end_with_yaojie}}
          },
          #submit_test_case{
              subtitle = "inning ends without Yaojie",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wen(9)]},
                      player1 => {[], [wen(10), wuF(8), wuT(8)]},
                      player2 => {[], [wuT(5), wuF(5)]},
                      player3 => {[wen(11)], []},
                      table => {single_wu, Exposed(wunumT(3), [closed, {open, wunumT(7)}])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wen(11)],
              expected =
                  {wins_inning, ?SEAT2,
                      {single_wu, Exposed(wunumT(3), [closed, {open, wunumT(7)}, closed])},
                      #{
                          east  => [wen(11), wen(9)],
                          south => [wen(10), wuF(8), wuT(8)],
                          west  => [wuT(5), wuF(5), wuT(7)],
                          north => []
                      },
                      error}
          },
          #submit_test_case{
              subtitle = "last trick after Qizhijie, third submission",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wuT(9), wuT(3), wuT(6), wen(8), wuF(5), wuT(5)]},
                      player1 => {[], []},
                      player2 => {[wen(11)], []},
                      player3 => {[wuF(9)], []},
                      table => {single_wen, Exposed(7, [closed])}
                  }),
              submitter_seat = ?SEAT2,
              submitter_cards = [wen(11)],
              expected =
                  {continues, inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wuT(9), wuT(3), wuT(6), wen(8), wuF(5), wuT(5)]},
                      player1 => {[], []},
                      player2 => {[], []},
                      player3 => {[wuF(9)], []},
                      table => {single_wen, Exposed(7, [closed, {open, 11}])}
                  })}
          },
          #submit_test_case{
              subtitle = "last trick after Qizhijie, last submission",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wuT(9), wuT(3), wuT(6), wen(8), wuF(5), wuT(5)]},
                      player1 => {[], []},
                      player2 => {[], []},
                      player3 => {[wuF(9)], []},
                      table => {single_wen, Exposed(7, [closed, {open, 11}])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wuF(9)],
              expected =
                  {wins_inning, ?SEAT0,
                      {single_wen, Exposed(7, [closed, {open, 11}, closed])},
                      #{
                          east  => [wen(11), wuT(9), wuT(3), wuT(6), wen(8), wuF(5), wuT(5)],
                          south => [],
                          west  => [wen(11)],
                          north => []
                      },
                      {ok, inning_end_with_qizhijie}}
          },
          #submit_test_case{
              subtitle = "last trick after Qizhijie, last submission (and achieves Bazhijie)",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wuT(9), wuT(3), wuT(6), wen(8), wuF(5), wuT(5)]},
                      player1 => {[], []},
                      player2 => {[], []},
                      player3 => {[wen(11)], []},
                      table => {single_wu, Exposed(wunumF(7), [closed, closed])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wen(11)],
              expected =
                  {wins_inning, ?SEAT0,
                      {single_wu, Exposed(wunumF(7), [closed, closed, closed])},
                      #{
                          east  => [wen(11), wuT(9), wuT(3), wuT(6), wen(8), wuF(5), wuT(5), wuF(7)],
                          south => [],
                          west  => [],
                          north => []
                      },
                      {ok, inning_end_with_bazhijie}}
          },
          #submit_test_case{
              subtitle = "last triple submission and achieves Bazhijie",
              before =
                  inning_state(#{
                      starts_at => ?SEAT0,
                      player0 => {[], [wen(11), wuT(9), wuT(3), wuT(6), wen(11)]},
                      player1 => {[], []},
                      player2 => {[], []},
                      player3 => {[wen(9), wen(11), wuF(9)], []},
                      table => {triple_wu, Exposed(big_a, [closed, closed])}
                  }),
              submitter_seat = ?SEAT3,
              submitter_cards = [wen(9), wen(11), wuF(9)],
              expected =
                  {wins_inning, ?SEAT0,
                      {triple_wu, Exposed(big_a, [closed, closed, closed])},
                      #{
                          east  => [wen(11), wuT(9), wuT(3), wuT(6), wen(11), wen(8), wuF(5), wuT(5)],
                          south => [],
                          west  => [],
                          north => []
                      },
                      {ok, inning_end_with_bazhijie}}
          }
      ]
    ].

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
wen(Wen) -> {wen, Wen}.
wuT(Wunum) -> {wu, wunumT(Wunum)}.
wuF(Wunum) -> {wu, wunumF(Wunum)}.
wunumT(Wunum) -> #{design => true, number => Wunum}.
wunumF(Wunum) -> #{design => false, number => Wunum}.
bigT(Big) -> #{design => true, main => Big}.
bigF(Big) -> #{design => false, main => Big}.

inning_state(#{
    starts_at := StartSeat,
    player0   := {Hand0, Gaineds0},
    player1   := {Hand1, Gaineds1},
    player2   := {Hand2, Gaineds2},
    player3   := {Hand3, Gaineds3},
    table     := TableState
}) ->
    PlayerQuad = #{
        east  => #{hand => Hand0, gained => Gaineds0},
        south => #{hand => Hand1, gained => Gaineds1},
        west  => #{hand => Hand2, gained => Gaineds2},
        north => #{hand => Hand3, gained => Gaineds3}
    },
    #{
        starts_at => StartSeat,
        players   => PlayerQuad,
        table     => TableState
    }.

%% `fun(Inning.submit_result) -> Inning.submit_result'
sort_hands_of_result(Result) ->
    case Result of
        {continues, Next} ->
            {continues, sort_hands(Next)};
        {wins_trick, WinnerSeat, LastTable, Hand, MaybeSpecial, Next} ->
            {wins_trick, WinnerSeat, LastTable, ?CARD_MODULE:sort(Hand), MaybeSpecial, sort_hands(Next)};
        {wins_inning, WinnerSeat, LastTable, GainQuad, MaybeSpecial} ->
            {wins_inning, WinnerSeat, LastTable, GainQuad, MaybeSpecial}
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
