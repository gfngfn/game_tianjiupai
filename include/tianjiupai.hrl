
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
