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

zip_with_indices_test_() ->
    [
     {"zip_with_indices/1",
      fun() ->
          ?assertEqual([{0, a}, {1, b}, {2, c}], tianjiupai_game:zip_with_indices([a, b, c]))
      end}
    ].

max_with_index_test_() ->
    [
     {"max_with_index/1",
      fun() ->
          ?assertEqual({2, d}, tianjiupai_game:max_with_index(fun(X1, X2) -> X1 > X2 end, [a, c, d, b]))
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

update_table_success_test_() ->
    First = fun(X) -> {X, []} end,
    Exposed = fun(X, XOrCloseds) -> {X, XOrCloseds} end,
    [
     {"Succeeds in updating table states.",
      fun() ->
          Got = tianjiupai_game:update_table(SubmittedCards, Table),
          ?assertEqual({ok, Expected}, Got)
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
