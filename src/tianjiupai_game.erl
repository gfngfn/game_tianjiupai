-module(tianjiupai_game).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    playing_state/0
]).
-export([
    get_user_ids/1,
    initial_state/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type card() ::
    {wen, non_neg_integer()}
  | {wu, non_neg_integer()}.

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

-record(playing_state, {
    parent    :: player_index(),  % Who is the parent of the current game.
    starts_at :: player_index(),  % Who has submitted the first card in the current trick.
    players   :: [#player{}],     % The list of players. Must be of length 4.
    table     :: table_state()
}).

-opaque playing_state() :: #playing_state{}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec get_user_ids(playing_state()) -> [tianjiupai:user_id()].
get_user_ids(Play) ->
    #playing_state{players = Players} = Play,
    lists:map(fun(#player{user_id = U}) -> U end, Players).

-spec initial_state(player_index(), [{tianjiupai:user_id(), [card()]}]) -> playing_state().
initial_state(Parent, UserIdsAndHands) ->
    #playing_state{
        parent    = Parent,
        starts_at = Parent,
        table     = starting,

        players =
            lists:map(
                fun({UserId, Hand}) ->
                    #player{
                        user_id   = UserId,
                        hand      = Hand,
                        gained    = []
                    }
                end,
                UserIdsAndHands)
    }.
