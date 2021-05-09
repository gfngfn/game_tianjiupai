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

    quad_map/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type quad(X) :: {X, X, X, X}.

-type card_wen() :: non_neg_integer().

-type card_wu() :: non_neg_integer().

-type card() ::
    {wen, card_wen()}
  | {wu, card_wu()}.

-type table_state() ::
    starting
  | {single, [card()]}
  | {double, [{card(), card()}]}
  | {triple, [{card(), card(), card()}]}
  | {quadruple, [{card(), card(), card(), card()}]}.

-record(player, {
    user_id   :: tianjiupai:user_id(),
    hand      :: [card()],  % The list of cards in the hand.
    gained    :: [card()]   % The list of obtained decks. Elements are the top of the deck.
}).

-type player_index() :: 0 | 1 | 2 | 3.

-record(inning_state, {
    parent    :: player_index(),  % Who is the parent of the current game.
    starts_at :: player_index(),  % Who has submitted the first card in the current trick.
    players   :: quad(#player{}),
    table     :: table_state()
}).

-opaque inning_state() :: #inning_state{}.

-define(WEN_CARDS_HALF, lists:map(fun(N) -> {wen, N} end, lists:seq(1, 11))).

-define(WU_CARDS, lists:map(fun(N) -> {wu, N} end, [3, 5, 5, 6, 7, 7, 8, 8, 9, 9])).

-define(ALL_CARDS, lists:append([?WEN_CARDS_HALF, ?WEN_CARDS_HALF, ?WU_CARDS])).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec get_user_ids(inning_state()) -> [tianjiupai:user_id()].
get_user_ids(Play) ->
    #inning_state{players = Players} = Play,
    {U0, U1, U2, U3} = quad_map(fun(#player{user_id = U}) -> U end, Players),
    [U0, U1, U2, U3].

-spec generate_initial_inning_state(player_index(), quad(tianjiupai:user_id())) -> inning_state().
generate_initial_inning_state(Parent, UserIdQuad) ->
    HandQuad = shuffle(),
    #inning_state{
        parent    = Parent,
        starts_at = Parent,
        table     = starting,

        players =
            quad_zip(
                fun(UserId, Hand) ->
                    #player{
                        user_id   = UserId,
                        hand      = Hand,
                        gained    = []
                    }
                end,
                UserIdQuad, HandQuad)
    }.

-spec shuffle() -> quad([card()]).
shuffle() ->
    %% RandomlyIndexedCards :: [{float(), card()}]
    RandomlyIndexedCards =
        lists:map(
            fun(Card) ->
                Index = rand:uniform(),
                {Index, Card}
            end,
            ?ALL_CARDS),
    Sorted0 =
        lists:sort(
            fun({Index1, _}, {Index2, _}) ->
                Index1 =< Index2
            end,
            RandomlyIndexedCards),
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
