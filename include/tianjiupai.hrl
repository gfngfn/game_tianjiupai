
%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(whole_room_state, {
    room_id    :: tianjiupai:room_id(),
    room_name  :: binary(),
    members    :: [tianjiupai:user_id()],
    is_playing :: boolean()
}).

-record(personal_room_state, {
    room_id    :: tianjiupai:room_id(),
    room_name  :: binary(),
    logs       :: [tianjiupai_room_server:log()],
    observable :: tianjiupai_room_server:observable_room_state()
}).

-record(observable_inning_state, {
    starts_at  :: tianjiupai_quad:seat(),
    your_hand  :: [tianjiupai_game:card()],
    gains      :: tianjiupai_quad:quad([tianjiupai_game:card()]),
    table      :: tianjiupai_game:table_state()
}).

-record(game_player, {
    user_id :: tianjiupai:user_id(),
    score   :: tianjiupai_room_server:score()
}).

-record(game_meta, {
    inning_index     :: pos_integer(),
    num_consecutives :: pos_integer(),
    parent_seat      :: tianjiupai_quad:seat(),
    players          :: tianjiupai_quad:quad(#game_player{})
}).

-record(observable_game_state, {
    meta              :: #game_meta{},
    observable_inning :: #observable_inning_state{},
    snapshot_id       :: tianjiupai_room_server:snapshot_id()
}).
