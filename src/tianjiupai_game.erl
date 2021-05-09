-module(tianjiupai_game).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    inning_state/0
]).
-export([
    generate_initial_inning_state/1,
    get_observable_inning_state/2,
    submit/3,

    %% For tests:
    all_cards/0,
    shuffle/0,
    make_starting_table/1,
    update_table/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type quad(X) :: tianjiupai_quad:quad(X).

-type seat() :: tianjiupai_quad:seat().

-type card_wen() :: non_neg_integer().

-type card_wu() :: non_neg_integer().

-type card_big() :: big1 | big2 | big3 | big4.

-type card() ::
    {wen, card_wen()}
  | {wu, card_wu()}.

-type closed_or(X) ::
    closed
  | {open, X}.

-type exposed(X) ::
  {X, [closed_or(X)]}.

-type table_state() ::
    starting
  | {wuzun,       exposed(ok)}
  | {wenzun,      exposed(minor | major)}
  | {single_wen,  exposed(card_wen())}
  | {single_wu,   exposed(card_wu())}
  | {double_wen,  exposed(card_wen())}
  | {double_wu,   exposed(card_wu())}
  | {double_both, exposed(card_big())}
  | {triple_wen,  exposed(card_big())}
  | {triple_wu,   exposed(card_big())}
  | {quadruple,   exposed(card_big())}.

-record(player, {
    hand      :: [card()],  % The list of cards in the hand.
    gained    :: [card()]   % The list of obtained decks. Elements are the top of the deck.
}).

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
    your_hand  :: [card()],
    gains      :: quad([card()]),
    table      :: table_state()
}).

-define(WEN_CARDS_HALF, lists:map(fun(N) -> {wen, N} end, lists:seq(1, 11))).

-define(WU_CARDS, lists:map(fun(N) -> {wu, N} end, [3, 5, 5, 6, 7, 7, 8, 8, 9, 9])).

-define(ALL_CARDS, lists:append([?WEN_CARDS_HALF, ?WEN_CARDS_HALF, ?WU_CARDS])).

-define(WU_NUMBER_THREE, 3).
-define(WU_NUMBER_FIVE,  5).
-define(WU_NUMBER_SIX,   6).
-define(WU_NUMBER_SEVEN, 7).
-define(WU_NUMBER_EIGHT, 8).
-define(WU_NUMBER_NINE,  9).

-define(WEN_NUMBER_TIEN,   11).
-define(WEN_NUMBER_DI,     10).
-define(WEN_NUMBER_REN,    9).
-define(WEN_NUMBER_HE,     8).
-define(WEN_NUMBER_BOOBY,  2).
-define(WEN_NUMBER_BOTTOM, 1).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec generate_initial_inning_state(seat()) -> inning_state().
generate_initial_inning_state(ParentSeat) ->
    HandQuad = shuffle(),
    #inning_state{
        parent    = ParentSeat,
        starts_at = ParentSeat,
        table     = starting,

        players =
            tianjiupai_quad:map(
                fun(Hand) ->
                    #player{
                        hand    = Hand,
                        gained  = []
                    }
                end,
                HandQuad)
    }.

-spec get_observable_inning_state(seat(), #inning_state{}) -> #observable_inning_state{}.
get_observable_inning_state(YourSeat, InningState) ->
    #inning_state{
        parent    = ParentSeat,
        starts_at = StartSeat,
        table     = Table,
        players   = PlayerQuad
    } = InningState,
    #player{hand = YourHand} = tianjiupai_quad:access(YourSeat, PlayerQuad),
    #observable_inning_state{
        parent     = ParentSeat,
        starts_at  = StartSeat,
        your_hand  = YourHand,
        gains      = tianjiupai_quad:map(fun(#player{gained = TopCards}) -> TopCards end, PlayerQuad),
        table      = Table
    }.

-spec submit(seat(), [card()], inning_state()) -> {ok, inning_state()} | {error, Reason} when
    Reason ::
        not_your_turn
      | submitter_does_not_own_submitted_cards
      | wrong_number_of_submitted_cards.
submit(SubmitterSeat, SubmittedCards, InningState) ->
    #inning_state{
        parent    = ParentSeat,
        starts_at = StartSeat,
        table     = Table0,
        players   = PlayerQuad0
    } = InningState,
    N = table_length(Table0),
    case SubmitterSeat =:= tianjiupai_quad:advance_seat(StartSeat, N) of
        true ->
            SubmittingPlayer0 = tianjiupai_quad:access(SubmitterSeat, PlayerQuad0),
            #player{hand = SubmitterHand0} = SubmittingPlayer0,
            case separate_submitted_cards(SubmitterHand0, SubmittedCards) of
                {ok, SubmitterHand1} ->
                    PlayerQuad1 =
                        tianjiupai_quad:update(
                            SubmitterSeat,
                            SubmittingPlayer0#player{hand = SubmitterHand1},
                            PlayerQuad0),
                    case update_table(SubmittedCards, Table0) of
                        {ok, Table1} ->
                            {ok, #inning_state{
                                parent    = ParentSeat,
                                starts_at = StartSeat,
                                table     = Table1,
                                players   = PlayerQuad1
                            }};
                        error ->
                            {error, wrong_number_of_submitted_cards}
                    end;
                error ->
                    {error, submitter_does_not_own_submitted_cards}
            end;
        false ->
            {error, not_your_turn}
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
%% @doc Returns how many players have already submitted cards in the current trick.
-spec table_length(table_state()) -> non_neg_integer().
table_length(Table) ->
    case Table of
        starting               -> 0;
        {_Tag, _X, XOrCloseds} -> 1 + erlang:length(XOrCloseds)
    end.

-spec separate_submitted_cards(
    SubmitterHandBefore :: [card()],
    SubmittedCards      :: [card()]
) ->
    {ok, SubmitterHandAfter :: [card()]}
  | error.
separate_submitted_cards(SubmitterHand0, SubmittedCards) ->
    SubmitterHand1 = SubmitterHand0 -- SubmittedCards,
    case erlang:length(SubmitterHand1) + erlang:length(SubmittedCards) =:= erlang:length(SubmitterHand0) of
        true  -> {ok, SubmitterHand1};
        false -> error
    end.

-spec update_table([card()], table_state()) -> {ok, table_state()} | error.
update_table(SubmittedCards, Table) ->
    case Table of
        starting ->
            make_starting_table(SubmittedCards);
        {wuzun, ExposedOk} ->
            case SubmittedCards of
                [_, _] ->
                    {ok, {wuzun, expose(ExposedOk, closed)}};
                _ ->
                    error
            end;
        {wenzun, Exposed} ->
            case SubmittedCards of
                [{wen, ?WEN_NUMBER_BOOBY}, {wen, ?WEN_NUMBER_BOOBY}] ->
                    {ok, {wenzun, expose(Exposed, {open, major})}};
                [_, _] ->
                    {ok, {wenzun, expose(Exposed, closed)}};
                _ ->
                    error
            end;
        {single_wen, ExposedWen} ->
            case SubmittedCards of
                [{wen, Wen}] ->
                    WenMax = wen_max(ExposedWen),
                    New =
                        case wen_greater(Wen, WenMax) of
                            true  -> {open, Wen};
                            false -> closed
                        end,
                    {ok, {single_wen, expose(ExposedWen, New)}};
                [_] ->
                    {ok, {single_wen, expose(ExposedWen, closed)}};
                _ ->
                    error
            end;
        {single_wu, ExposedWu} ->
            case SubmittedCards of
                [{wu, Wu}] ->
                    WuMax = wu_max(ExposedWu),
                    New =
                        case wu_greater(Wu, WuMax) of
                            true  -> {open, Wu};
                            false -> closed
                        end,
                    {ok, {single_wu, expose(ExposedWu, New)}};
                [_] ->
                    {ok, {single_wu, expose(ExposedWu, closed)}};
                _ ->
                    error
            end;
        {double_wen, ExposedWen} ->
            case SubmittedCards of
                [{wen, Wen}, {wen, Wen}] ->
                    WenMax = wen_max(ExposedWen),
                    New =
                        case wen_greater(Wen, WenMax) of
                            true  -> {open, Wen};
                            false -> closed
                        end,
                    {ok, {double_wen, expose(ExposedWen, New)}};
                [_, _] ->
                    {ok, {double_wen, expose(ExposedWen, closed)}};
                _ ->
                    error
            end;
        {double_wu, ExposedWu} ->
            case SubmittedCards of
                [{wu, Wu}, {wu, Wu}] ->
                    WuMax = wu_max(ExposedWu),
                    New =
                        case wu_greater(Wu, WuMax) of
                            true  -> {open, Wu};
                            false -> closed
                        end,
                    {ok, {double_wu, expose(ExposedWu, New)}};
                [_, _] ->
                    {ok, {double_wu, expose(ExposedWu, closed)}};
                _ ->
                    error
            end;
        {double_both, ExposedBig} ->
            case sort_cards(SubmittedCards) of
                [{wen, Wen}, {wu, Wu}] ->
                    update_both(double_both, ExposedBig, Wen, Wu);
                [_, _] ->
                    {ok, {double_both, expose(ExposedBig, closed)}};
                _ ->
                    error
            end;
        {triple_wen, ExposedBig} ->
            case sort_cards(SubmittedCards) of
                [{wen, Wen}, {wen, Wen}, {wu, Wu}] ->
                    update_both(triple_wen, ExposedBig, Wen, Wu);
                [_, _, _] ->
                    {ok, {triple_wen, expose(ExposedBig, closed)}};
                _ ->
                    error
            end;
        {triple_wu, ExposedBig} ->
            case sort_cards(SubmittedCards) of
                [{wen, Wen}, {wu, Wu}, {wu, Wu}] ->
                    update_both(triple_wu, ExposedBig, Wen, Wu);
                [_, _, _] ->
                    {ok, {triple_wu, expose(ExposedBig, closed)}};
                _ ->
                    error
            end;
        {quadruple, ExposedBig} ->
            case sort_cards(SubmittedCards) of
                [{wen, Wen}, {wen, Wen}, {wu, Wu}, {wu, Wu}] ->
                    update_both(quadruple, ExposedBig, Wen, Wu);
                [_, _, _, _] ->
                    {ok, {quadruple, expose(ExposedBig, closed)}};
                _ ->
                    error
            end
    end.

-spec update_both(Tag, exposed(card_big()), card_wen(), card_wu()) -> {ok, table_state()} | error when
    Tag :: double_both | triple_wen | triple_wu | quadruple.
update_both(Tag, ExposedBig, Wen, Wu) ->
    case wen_and_wu_to_big(Wen, Wu) of
        {ok, Big} ->
            BigMax = big_max(ExposedBig),
            New =
                case big_greater(Big, BigMax) of
                    true  -> {open, Big};
                    false -> closed
                end,
            {ok, {Tag, expose(ExposedBig, New)}};
        error ->
            {ok, {Tag, expose(ExposedBig, closed)}}
    end.

%% @doc Returns the current maximum big card.
-spec big_max(exposed(card_big())) -> card_big().
big_max({Big0, BigOrCloseds}) ->
    lists:max([Big0 | [Big || {open, Big} <- BigOrCloseds]]).

-spec big_greater(card_big(), card_big()) -> boolean().
big_greater(Big1, Big2) ->
    Big1 > Big2.

%% @doc Returns the current maximum wen card.
-spec wen_max(exposed(card_wen())) -> card_wen().
wen_max({Wen0, WenOrCloseds}) ->
    lists:max([Wen0 | [Wen || {open, Wen} <- WenOrCloseds]]).

-spec wen_greater(card_wen(), card_wen()) -> boolean().
wen_greater(Wen1, Wen2) ->
    Wen1 > Wen2.

%% @doc Returns the current maximum wu card.
-spec wu_max(exposed(card_wu())) -> card_wu().
wu_max({Wu0, WuOrCloseds}) ->
    lists:max([Wu0 | [Wu || {open, Wu} <- WuOrCloseds]]).

-spec wu_greater(card_wu(), card_wu()) -> boolean().
wu_greater(Wu1, Wu2) ->
    Wu1 > Wu2.

%% @doc Makes a table state according to the first submission.
%%
%% Here, all given cards can be assumed to be in the valid form.
-spec make_starting_table([card()]) -> {ok, table_state()} | error.
make_starting_table(SubmittedCards) ->
    case sort_cards(SubmittedCards) of
        [{wu, ?WU_NUMBER_THREE}, {wu, ?WU_NUMBER_SIX}] ->
            {ok, {wuzun, first_exposed(ok)}};
        [{wen, ?WEN_NUMBER_BOTTOM}, {wen, ?WEN_NUMBER_BOTTOM}] ->
            {ok, {wenzun, first_exposed(minor)}};
        [{wen, Wen}] ->
            {ok, {single_wen, first_exposed(Wen)}};
        [{wu, Wu}] ->
            {ok, {single_wu, first_exposed(Wu)}};
        [{wen, Wen}, {wen, Wen}] ->
            {ok, {double_wen, first_exposed(Wen)}};
        [{wu, Wu}, {wu, Wu}] ->
            {ok, {double_wu, first_exposed(Wu)}};
        [{wen, Wen}, {wu, Wu}] ->
            case wen_and_wu_to_big(Wen, Wu) of
                {ok, Big} -> {ok, {double_both, first_exposed(Big)}};
                error     -> error
            end;
        [{wen, Wen}, {wen, Wen}, {wu, Wu}] ->
            case wen_and_wu_to_big(Wen, Wu) of
                {ok, Big} -> {ok, {triple_wen, first_exposed(Big)}};
                error     -> error
            end;
        [{wen, Wen}, {wu, Wu}, {wu, Wu}] ->
            case wen_and_wu_to_big(Wen, Wu) of
                {ok, Big} -> {ok, {triple_wu, first_exposed(Big)}};
                error     -> error
            end;
        [{wen, Wen}, {wen, Wen}, {wu, Wu}, {wu, Wu}] ->
            case wen_and_wu_to_big(Wen, Wu) of
                {ok, Big} -> {ok, {quadruple, first_exposed(Big)}};
                error     -> error
            end;
        _ ->
            error
    end.

-spec first_exposed(X) -> exposed(X) when
    X :: term().
first_exposed(X) ->
    {X, []}.

-spec expose(exposed(X), closed_or(X)) -> exposed(X) when
    X :: term().
expose({X, XOrCloseds}, New) ->
    {X, XOrCloseds ++ [New]}.

-spec wen_and_wu_to_big(card_wen(), card_wu()) -> {ok, card_big()} | error.
wen_and_wu_to_big(Wen, Wu) ->
    case {wen_to_big(Wen), wu_to_big(Wu)} of
        {{ok, Big}, {ok, Big}} -> {ok, Big};
        _                      -> error
    end.

-spec wen_to_big(card_wen()) -> {ok, card_big()} | error.
wen_to_big(Wen) ->
    case Wen of
        ?WEN_NUMBER_TIEN -> {ok, big4};
        ?WEN_NUMBER_DI   -> {ok, big3};
        ?WEN_NUMBER_REN  -> {ok, big2};
        ?WEN_NUMBER_HE   -> {ok, big1};
        _                -> error
    end.

-spec wu_to_big(card_wu()) -> {ok, card_big()} | error.
wu_to_big(Wu) ->
    case Wu of
        ?WU_NUMBER_NINE  -> {ok, big4};
        ?WU_NUMBER_EIGHT -> {ok, big3};
        ?WU_NUMBER_SEVEN -> {ok, big2};
        ?WU_NUMBER_FIVE  -> {ok, big1};
        _                -> error
    end.

%% @doc Sorts given cards in the lexicographical order.
%%
%% Wen takes precedence to wu.
-spec sort_cards([card()]) -> card().
sort_cards(Cards) ->
    lists:sort(Cards).

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
