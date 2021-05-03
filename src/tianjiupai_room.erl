-module(tianjiupai_room).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    room_id/0,
    player_index/0
]).
-export([
    create/1,
    attend/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type room_id() :: binary().

-type player_index() :: non_neg_integer().

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec create(RoomName :: binary()) -> {ok, room_id()} | {error, Reason :: term()}.
create(RoomName) ->
    RoomId = generate_room_id(),
    case tianjiupai_room_server_sup:start_child(RoomId, RoomName) of
        {ok, _Pid}       -> {ok, RoomId};
        {error, _} = Err -> Err
    end.

-spec attend(room_id(), tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
attend(RoomId, UserId) ->
    tianjiupai_room_server:attend(RoomId, UserId).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_room_id() -> binary().
generate_room_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
