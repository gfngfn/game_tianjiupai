-module(tianjiupai_room).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    create/1,
    get_all_rooms/0,
    attend/2,
    exit/2,
    send_chat/3,
    monitor/1
]).
-export_type([
    room_state/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type room_state() :: tianjiupai_room_server:room_state().

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

-spec get_all_rooms() -> [room_state()].
get_all_rooms() ->
    RoomServerProcs = tianjiupai_room_server_sup:which_children(),
    lists:filtermap(
        fun(RoomServerProc) ->
            case tianjiupai_room_server:get_state_by_proc(RoomServerProc) of
                {ok, RoomState}  -> {true, RoomState};
                {error, _Reason} -> false
            end
        end,
        RoomServerProcs).

-spec attend(tianjiupai:room_id(), tianjiupai:user_id()) -> {ok, room_state()} | {error, Reason :: term()}.
attend(RoomId, UserId) ->
    tianjiupai_room_server:attend(RoomId, UserId).

-spec exit(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
exit(RoomId, UserId) ->
    tianjiupai_room_server:exit(RoomId, UserId).

send_chat(RoomId, From, Text) ->
    tianjiupai_room_server:send_chat(RoomId, From, Text).

-spec monitor(tinajiupai:room_id()) -> {ok, reference()} | {error, {room_not_found, tianjiupai:room_id()}}.
monitor(RoomId) ->
    tianjiupai_room_server:monitor(RoomId).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_room_id() -> binary().
generate_room_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
