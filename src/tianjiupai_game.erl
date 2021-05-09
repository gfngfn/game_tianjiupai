-module(tianjiupai_game).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    inning_state/0
]).
-export([
    get_user_ids/1,
    generate_initial_inning_state/2,
    get_observable_inning_state/2,

    %% For tests:
    all_cards/0,
    shuffle/0,
    quad_map/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type quad(X) :: {X, X, X, X}.

-type card_wen() :: non_neg_integer().

-type card_wu() :: non_neg_integer().

-type card_big() :: big1 | big2 | big3 | big4.

-type card() ::
    {wen, card_wen()}
  | {wu, card_wu()}.

-type closed_or(X) ::
    closed
  | {open, X}.

-type table_state() ::
    starting
  | {single_wen,  [closed_or(card_wen())]}
  | {single_wu,   [closed_or(card_wu())]}
  | {double_wen,  [closed_or(card_wen())]}
  | {double_wu,   [closed_or(card_wu())]}
  | {double_both, [closed_or(card_big())]}
  | {triple_wen,  [closed_or(card_big())]}
  | {triple_wu,   [closed_or(card_big())]}
  | {quadruple,   [closed_or(card_big())]}
  | {zhizun,      [closed_or(zhizun)]}.

-record(player, {
    user_id   :: tianjiupai:user_id(),
    hand      :: [card()],  % The list of cards in the hand.
    gained    :: [card()]   % The list of obtained decks. Elements are the top of the deck.
}).

-type seat() :: 0 | 1 | 2 | 3.

-record(inning_state, {
    parent    :: seat(),  % Who is the parent of the current game.
    starts_at :: seat(),  % Who has submitted the first card in the current trick.
    players   :: quad(#player{}),
    table     :: table_state()
}).

-opaque inning_state() :: #inning_state{}.

-record(observable_inning_state, {
    parent     :: seat(),
    starts_at  :: seat(),
    you_are_at :: seat(),
    your_hand  :: [card()],
    gains      :: quad([card()]),
    table      :: table_state()
}).

-define(WEN_CARDS_HALF, lists:map(fun(N) -> {wen, N} end, lists:seq(1, 11))).

-define(WU_CARDS, lists:map(fun(N) -> {wu, N} end, [3, 5, 5, 6, 7, 7, 8, 8, 9, 9])).

-define(ALL_CARDS, lists:append([?WEN_CARDS_HALF, ?WEN_CARDS_HALF, ?WU_CARDS])).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec get_user_ids(inning_state()) -> [tianjiupai:user_id()].
get_user_ids(InningState) ->
    #inning_state{players = PlayerQuad} = InningState,
    {U0, U1, U2, U3} = quad_map(fun(#player{user_id = U}) -> U end, PlayerQuad),
    [U0, U1, U2, U3].

-spec generate_initial_inning_state(seat(), quad(tianjiupai:user_id())) -> inning_state().
generate_initial_inning_state(ParentSeat, UserIdQuad) ->
    HandQuad = shuffle(),
    #inning_state{
        parent    = ParentSeat,
        starts_at = ParentSeat,
        table     = starting,

        players =
            quad_zip(
                fun(UserId, Hand) ->
                    #player{
                        user_id = UserId,
                        hand    = Hand,
                        gained  = []
                    }
                end,
                UserIdQuad, HandQuad)
    }.

-spec get_observable_inning_state(tianjiupai:user_id(), #inning_state{}) ->
    {ok, #observable_inning_state{}}
  | error.
get_observable_inning_state(UserId, InningState) ->
    #inning_state{
        parent    = ParentSeat,
        starts_at = StartSeat,
        table     = Table,
        players   = PlayerQuad
    } = InningState,
    case quad_find(fun(#player{user_id = U}) -> U =:= UserId end, PlayerQuad) of
        {ok, {You, YourSeat}} ->
            #player{hand = YourHand} = You,
            {#observable_inning_state{
                parent     = ParentSeat,
                starts_at  = StartSeat,
                you_are_at = YourSeat,
                your_hand  = YourHand,
                gains      = quad_map(fun(#player{gained = TopCards}) -> TopCards end, PlayerQuad),
                table      = Table
            }};
        error ->
            error
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec all_cards() -> [card()].
all_cards() ->
    ?ALL_CARDS.

-spec shuffle() -> quad([card()]).
shuffle() ->
    %% RandomlyIndexedCards :: [{float(), card()}]
    RandomlyIndexedCards =
        lists:map(
            fun(Card) ->
                Index = rand:uniform(),
                {Index, Card}
            end,
            all_cards()),
    SortedIndexedCards =
        lists:sort(
            fun({Index1, _}, {Index2, _}) ->
                Index1 =< Index2
            end,
            RandomlyIndexedCards),
    Sorted0 = lists:map(fun({_, Card}) -> Card end, SortedIndexedCards),
    {Hand0, Sorted1} = lists:split(8, Sorted0),
    {Hand1, Sorted2} = lists:split(8, Sorted1),
    {Hand2, Sorted3} = lists:split(8, Sorted2),
    {Hand3, []}      = lists:split(8, Sorted3),
    {Hand0, Hand1, Hand2, Hand3}.

-spec quad_zip(F, quad(X), quad(Y)) -> quad(Z) when
    F :: fun((X, Y) -> Z),
    X :: term(),
    Y :: term(),
    Z :: term().
quad_zip(F, {X0, X1, X2, X3}, {Y0, Y1, Y2, Y3}) ->
    {F(X0, Y0), F(X1, Y1), F(X2, Y2), F(X3, Y3)}.

-spec quad_map(F, quad(X)) -> quad(Y) when
    F :: fun((X) -> Y),
    X :: term(),
    Y :: term().
quad_map(F, {X0, X1, X2, X3}) ->
    {F(X0), F(X1), F(X2), F(X3)}.

-spec quad_find(F, quad(X)) -> {ok, {X, seat()}} | error when
    F :: fun((X) -> boolean()),
    X :: term().
quad_find(F, Quad) ->
    {X0, X1, X2, X3} = Quad,
    case {F(X0), F(X1), F(X2), F(X3)} of
        {true, _, _, _} -> {ok, {X0, 0}};
        {_, true, _, _} -> {ok, {X1, 1}};
        {_, _, true, _} -> {ok, {X2, 2}};
        {_, _, _, true} -> {ok, {X3, 3}};
        _               -> error
    end.
