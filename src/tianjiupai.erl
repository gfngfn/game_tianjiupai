-module(tianjiupai).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    seat/0,
    quad/1,
    card/0,
    snapshot_id/0,
    user_id/0,
    room_id/0,
    log/0,
    whole_room_state/0,
    game_player/0,
    table_state/0,
    game_meta/0,
    observable_inning_state/0,
    observable_game_state/0,
    observable_room_state/0,
    personal_room_state/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
%% See `Quad.seat'
-type seat() :: seat0 | seat1 | seat2 | seat3.

%% See `Quad.t'
-type quad(X) :: {X, X, X, X}.

%% See `Card.t'
-type card() :: term().

%% See `SnapshotId.t'
-type snapshot_id() :: binary().

%% See `Types.user_id'
-type user_id() :: binary().

%% See `Types.room_id'
-type room_id() :: binary().

%% See `Types.log'
-type log() ::
    {log_comment, user_id(), binary()}
  | {log_entered, user_id()}
  | {log_exited, user_id()}
  | log_game_start.

%% See `Types.whole_room_state'
-type whole_room_state() :: #{
    room_id    := room_id(),
    room_name  := binary(),
    members    := [user_id()],
    is_playing := boolean()
}.

-type game_player() :: #{
    user_id := user_id(),
    score   := non_neg_integer()
}.

%% See `Types.table_state'
-type table_state() :: term().

-type game_meta() :: #{
    inning_index     := non_neg_integer(),
    num_consecutives := non_neg_integer(),
    parent_seat      := seat(),
    players          := quad(game_player())
}.

-type observable_inning_state() :: #{
    starts_at := seat(),
    your_hand := [card()],
    gains     := quad([card()]),
    table     := table_state()
}.

-type observable_game_state() :: #{
    meta              := game_meta(),
    observable_inning := observable_inning_state(),
    snapshot_id       := snapshot_id()
}.

-type observable_room_state() ::
    {waiting, [user_id()]}
  | {playing, observable_game_state()}.

%% See `Types.personal_room_state'
-type personal_room_state() :: #{
    room_id    := room_id(),
    room_name  := binary(),
    logs       := [log()],
    observable := observable_room_state()
}.
