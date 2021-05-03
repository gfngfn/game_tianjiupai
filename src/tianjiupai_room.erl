-module(tianjiupai_room).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    create/1,
    attend/2,
    exit/2,
    monitor/1
]).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec create(RoomName :: binary()) -> {ok, tianjiupai:room_id()} | {error, Reason :: term()}.
create(RoomName) ->
    RoomId = generate_room_id(),
    case tianjiupai_room_server_sup:start_child(RoomId, RoomName) of
        {ok, _Pid}       -> {ok, RoomId};
        {error, _} = Err -> Err
    end.

-spec attend(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
attend(RoomId, UserId) ->
    tianjiupai_room_server:attend(RoomId, UserId).

-spec exit(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
exit(RoomId, UserId) ->
    tianjiupai_room_server:exit(RoomId, UserId).

-spec monitor(tinajiupai:room_id()) -> reference().
monitor(RoomId) ->
    tianjiupai_room_server:monitor(RoomId).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_room_id() -> binary().
generate_room_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
