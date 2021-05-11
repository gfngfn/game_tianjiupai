-module(tianjiupai_game).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    card/0,
    table_state/0,
    inning_state/0,
    submit_result/0
]).
-export([
    generate_initial_inning_state/1,
    get_observable_inning_state/2,
    submit/3,

    %% For tests:
    all_cards/0,
    shuffle/0,
    zip_with_indices/1,
    max_with_index/2,
    make_starting_table/1,
    update_table/2,
    get_winner/1,
    sort_cards/1
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
    starts_at :: seat(),  % Who has submitted the first card in the current trick.
    players   :: quad(#player{}),
    table     :: table_state()
}).

-opaque inning_state() :: #inning_state{}.

-record(observable_inning_state, {
    starts_at  :: seat(),
    your_hand  :: [card()],
    gains      :: quad([card()]),
    table      :: table_state()
}).

-type submit_result() ::
    {continues, Next :: inning_state()}
  | {wins_trick, WinnerSeat :: seat(), Next :: inning_state()}
  | {wins_inning, WinnerSeat :: seat(), NumGainedsQuad :: quad([card()])}.

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

-define(DUMMY_WEN_NULL, 0).
-define(DUMMY_WU_NULL, 0).
-define(DUMMY_BIG_NULL, big0).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec generate_initial_inning_state(seat()) -> inning_state().
generate_initial_inning_state(ParentSeat) ->
    HandQuad = shuffle(),
    #inning_state{
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
        starts_at = StartSeat,
        table     = Table,
        players   = PlayerQuad
    } = InningState,
    #player{hand = YourHand} = tianjiupai_quad:access(YourSeat, PlayerQuad),
    #observable_inning_state{
        starts_at  = StartSeat,
        your_hand  = YourHand,
        gains      = tianjiupai_quad:map(fun(#player{gained = TopCards}) -> TopCards end, PlayerQuad),
        table      = Table
    }.

-spec submit(seat(), [card()], inning_state()) -> {ok, submit_result()} | {error, Reason} when
    Reason ::
        not_your_turn
      | submitter_does_not_own_submitted_cards
      | wrong_number_of_submitted_cards.
submit(SubmitterSeat, SubmittedCards, InningState) ->
    #inning_state{
        starts_at = StartSeat,
        table     = Table0,
        players   = PlayerQuad0
    } = InningState,
    NumGaineds0 = count_gained(PlayerQuad0),
    NumSubmittedCards = erlang:length(SubmittedCards),
    N0 = table_length(Table0),
    case
        tianjiupai_quad:is_seat(StartSeat)
        andalso N0 < 4
        andalso NumGaineds0 < 8
        andalso 0 < NumSubmittedCards andalso NumSubmittedCards =< 4
        %% Validates inning states here just in case.
        %% Since `inning_state()' is an opaque type,
        %% there's no possibility that a given inning state is malformed,
        %% as long as the implementation of this module does not contain any flaw.
    of
        true ->
            case SubmitterSeat =:= tianjiupai_quad:advance_seat(StartSeat, N0) of
                true ->
                    SubmittingPlayer0 = tianjiupai_quad:access(SubmitterSeat, PlayerQuad0),
                    #player{hand = SubmitterHand0, gained = SubmitterGained0} = SubmittingPlayer0,
                    case separate_submitted_cards(SubmitterHand0, SubmittedCards) of
                        {ok, SubmitterHand1} ->
                            PlayerQuad1 =
                                tianjiupai_quad:update(
                                    SubmitterSeat,
                                    SubmittingPlayer0#player{hand = SubmitterHand1},
                                    PlayerQuad0),
                            Table1Result =
                                case NumGaineds0 =:= 7 andalso NumSubmittedCards =:= 1 of
                                    true ->
                                    %% If this is the last trick with single submissions:
                                        case erlang:length(SubmitterGained0) of
                                            0 ->
                                            %% If the submitter gains no card,
                                            %% the submitter does not have a right to attend this trick.
                                                {Tag, {X, XOrCloseds}} = Table0,
                                                %% `Table0' must be other than `starting' here,
                                                %% Since the first submitter of a trick after the first trick
                                                %% must gain at least one card.
                                                {ok, {Tag, {X, XOrCloseds ++ [closed]}}};
                                            _ ->
                                            %% If the submitter gains at least one card so far:
                                                update_table(SubmittedCards, Table0)
                                        end;
                                    false ->
                                        update_table(SubmittedCards, Table0)
                                end,
                            case Table1Result of
                                {ok, Table1} ->
                                    NumGaineds1 = NumGaineds0 + NumSubmittedCards,
                                    case N0 of
                                        3 ->
                                        %% If this is the last submission within a trick:
                                            {WinnerTrickIndex, Cards} = get_winner(Table1),
                                            WinnerSeat = tianjiupai_quad:advance_seat(StartSeat, WinnerTrickIndex),
                                            %% Winner :: #player{}
                                            Winner = tianjiupai_quad:access(WinnerSeat, PlayerQuad1),
                                            #player{gained = WinnerGained} = Winner,
                                            PlayerQuad2 =
                                                tianjiupai_quad:update(
                                                    WinnerSeat,
                                                    Winner#player{gained = WinnerGained ++ Cards},
                                                    PlayerQuad1),
                                            case NumGaineds1 of
                                                8 ->
                                                %% If this is the last trick within an inning:
                                                    GainedQuad =
                                                        tianjiupai_quad:map(
                                                            fun(#player{gained = Gained}) -> Gained end,
                                                            PlayerQuad2),
                                                    {ok, {wins_inning, WinnerSeat, GainedQuad}};
                                                _ ->
                                                    InningState1 =
                                                        #inning_state{
                                                             starts_at = WinnerSeat,
                                                             table     = starting,
                                                             players   = PlayerQuad2
                                                        },
                                                    {ok, {wins_trick, WinnerSeat, InningState1}}
                                            end;
                                        _ ->
                                            InningState1 =
                                                #inning_state{
                                                    starts_at = StartSeat,
                                                    table     = Table1,
                                                    players   = PlayerQuad1
                                                },
                                            {ok, {continues, InningState1}}
                                    end;
                                error ->
                                    {error, wrong_number_of_submitted_cards}
                            end;
                        error ->
                            {error, submitter_does_not_own_submitted_cards}
                    end;
                false ->
                    {error, not_your_turn}
            end;
        false ->
            {error, malformed_inning_state}
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec get_winner(table_state()) -> {WinnerTrickIndex :: non_neg_integer(), Cards :: [card()]}.
get_winner(Table) ->
    case Table of
        {wuzun, {ok, [closed, closed, closed]}} ->
            {0, [{wu, ?WU_NUMBER_THREE}, {wu, ?WU_NUMBER_SIX}]};
        {wenzun, {minor, MajorOrCloseds}} ->
            Ns =
                lists:map(
                    fun({open, major}) -> 2;
                       (closed)        -> 0
                    end,
                    MajorOrCloseds),
            {TrickIndex, OneOrTwo} =
                max_with_index(
                   fun(N1, N2) -> N1 > N2 end,
                   [1 | Ns]),
            case OneOrTwo of
                1 -> {0, [{wen, ?WEN_NUMBER_BOTTOM}, {wen, ?WEN_NUMBER_BOTTOM}]};
                2 -> {TrickIndex, [{wen, ?WEN_NUMBER_BOOBY}, {wen, ?WEN_NUMBER_BOOBY}]}
            end;
        {single_wen, WenExposed} ->
            {TrickIndex, Wen} = wen_max(WenExposed),
            {TrickIndex, [{wen, Wen}]};
        {single_wu, WuExposed} ->
            {TrickIndex, Wu} = wu_max(WuExposed),
            {TrickIndex, [{wu, Wu}]};
        {double_wen, WenExposed} ->
            {TrickIndex, Wen} = wen_max(WenExposed),
            {TrickIndex, [{wen, Wen}, {wen, Wen}]};
        {double_wu, WuExposed} ->
            {TrickIndex, Wu} = wu_max(WuExposed),
            {TrickIndex, [{wu, Wu}, {wu, Wu}]};
        {triple_wen, BigExposed} ->
            {TrickIndex, Big} = big_max(BigExposed),
            {Wen, Wu} = big_to_wen_and_wu(Big),
            {TrickIndex, [{wen, Wen}, {wen, Wen}, {wu, Wu}]};
        {triple_wu, BigExposed} ->
            {TrickIndex, Big} = big_max(BigExposed),
            {Wen, Wu} = big_to_wen_and_wu(Big),
            {TrickIndex, [{wen, Wen}, {wu, Wu}, {wu, Wu}]};
        {quadruple, BigExposed} ->
            {TrickIndex, Big} = big_max(BigExposed),
            {Wen, Wu} = big_to_wen_and_wu(Big),
            {TrickIndex, [{wen, Wen}, {wen, Wen}, {wu, Wu}, {wu, Wu}]}
    end.

-spec count_gained(quad(#player{})) -> non_neg_integer().
count_gained(PlayerQuad) ->
    {N0, N1, N2, N3} =
        tianjiupai_quad:map(
            fun(#player{gained = Cards}) -> erlang:length(Cards) end,
            PlayerQuad),
    N0 + N1 + N2 + N3.

%% @doc Returns how many players have already submitted cards in the current trick.
-spec table_length(table_state()) -> non_neg_integer().
table_length(Table) ->
    case Table of
        starting                 -> 0;
        {_Tag, {_X, XOrCloseds}} -> 1 + erlang:length(XOrCloseds)
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

%% @doc Updates table states by given submitted cards.
%%
%% The representation of submitted cards must be proven to be valid beforehand,
%% which has been done by `separate_submitted_cards'.
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
                    {_, WenMax} = wen_max(ExposedWen),
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
                    {_, WuMax} = wu_max(ExposedWu),
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
                    {_, WenMax} = wen_max(ExposedWen),
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
                    {_, WuMax} = wu_max(ExposedWu),
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
            {_, BigMax} = big_max(ExposedBig),
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
-spec big_max(exposed(card_big())) -> {TrickIndex :: non_neg_integer(), card_big()}.
big_max({Big0, BigOrCloseds}) ->
    %% Bigs :: card_big() | ?DUMMY_BIG_NULL
    Bigs =
        lists:map(
            fun({open, Big}) -> Big;
               (closed)      -> ?DUMMY_BIG_NULL
            end,
            BigOrCloseds),
    max_with_index(
        fun big_greater/2,
        [Big0 | Bigs]).

-spec big_greater(card_big() | ?DUMMY_BIG_NULL, card_big() | ?DUMMY_BIG_NULL) -> boolean().
big_greater(Big1, Big2) ->
    Big1 > Big2.

%% @doc Returns the current maximum wen card.
-spec wen_max(exposed(card_wen())) -> {TrickIndex :: non_neg_integer(), card_wen()}.
wen_max({Wen0, WenOrCloseds}) ->
    Wens =
        lists:map(
            fun({open, Wen}) -> Wen;
               (closed)      -> ?DUMMY_WEN_NULL
            end,
            WenOrCloseds),
    max_with_index(
        fun wen_greater/2,
        [Wen0 | Wens]).

-spec wen_greater(card_wen() | ?DUMMY_WEN_NULL, card_wen() | ?DUMMY_WEN_NULL) -> boolean().
wen_greater(Wen1, Wen2) ->
    Wen1 > Wen2.

%% @doc Returns the current maximum wu card.
-spec wu_max(exposed(card_wu())) -> {TrickIndex :: non_neg_integer(), card_wu()}.
wu_max({Wu0, WuOrCloseds}) ->
    Wus =
        lists:map(
            fun({open, Wu}) -> Wu;
               (closed)     -> ?DUMMY_WU_NULL
            end,
            WuOrCloseds),
    max_with_index(
        fun wu_greater/2,
        [Wu0 | Wus]).

-spec wu_greater(card_wu() | ?DUMMY_WU_NULL, card_wu() | ?DUMMY_WU_NULL) -> boolean().
wu_greater(Wu1, Wu2) ->
    Wu1 > Wu2.

-spec zip_with_indices([X]) -> [{TrickIndex :: non_neg_integer(), X}] when
    X :: term().
zip_with_indices(Xs) ->
    lists:zip(lists:seq(0, erlang:length(Xs) - 1), Xs).

-spec max_with_index(Greater, nonempty_list(X)) -> {TrickIndex :: non_neg_integer(), X} when
    Greater :: fun((X, X) -> boolean()),
    X       :: term().
max_with_index(Greater, Xs) ->
    Pairs = zip_with_indices(Xs),
    case
        lists:sort(
           fun({_, X1}, {_, X2}) -> Greater(X1, X2) orelse X1 =:= X2 end,
           Pairs)
    of
        [Pair | _] -> Pair
    end.


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

-spec big_to_wen_and_wu(card_big()) -> {card_wen(), card_wu()}.
big_to_wen_and_wu(Big) ->
    case Big of
        big4 -> {?WEN_NUMBER_TIEN, ?WU_NUMBER_NINE};
        big3 -> {?WEN_NUMBER_DI,   ?WU_NUMBER_EIGHT};
        big2 -> {?WEN_NUMBER_REN,  ?WU_NUMBER_SEVEN};
        big1 -> {?WEN_NUMBER_HE,   ?WU_NUMBER_FIVE}
    end.

%% @doc Sorts given cards in the lexicographical order.
%%
%% Wen takes precedence to wu.
-spec sort_cards([card()]) -> [card()].
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
