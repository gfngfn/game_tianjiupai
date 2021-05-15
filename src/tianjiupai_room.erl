-module(tianjiupai_room).

-include("tianjiupai.hrl").

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    create/1,
    get_all_rooms/0,
    get_whole_state/1,
    get_personal_state/2,
    attend/2,
    exit/2,
    send_chat/3,
    monitor/1
]).
-export_type([
    log/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type log() :: tianjiupai_room_server:log().

-define(ROOM_SERVER_MODULE, 'Tianjiupai.RoomServer').

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

-spec get_all_rooms() -> [#whole_room_state{}].
get_all_rooms() ->
    RoomServerProcs = tianjiupai_room_server_sup:which_children(),
    lists:filtermap(
        fun(RoomServerProc) ->
            case ?ROOM_SERVER_MODULE:get_whole_state_by_proc(RoomServerProc) of
                {ok, WholeRoomState} -> {true, WholeRoomState};
                {error, _Reason}     -> false
            end
        end,
        RoomServerProcs).

-spec get_whole_state(tianjiupai:room_id()) -> {ok, #whole_room_state{}} | {error, Reason :: term()}.
get_whole_state(RoomId) ->
    ?ROOM_SERVER_MODULE:get_whole_state(RoomId).

-spec get_personal_state(tianjiupai:room_id(), tianjiupai:user_id()) -> {ok, #personal_room_state{}} | {error, Reason :: term()}.
get_personal_state(RoomId, UserId) ->
    ?ROOM_SERVER_MODULE:get_personal_state(RoomId, UserId).

-spec attend(tianjiupai:room_id(), tianjiupai:user_id()) -> {ok, #personal_room_state{}} | {error, Reason :: term()}.
attend(RoomId, UserId) ->
    ?ROOM_SERVER_MODULE:attend(RoomId, UserId).

-spec exit(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
exit(RoomId, UserId) ->
    ?ROOM_SERVER_MODULE:exit(RoomId, UserId).

send_chat(RoomId, From, Text) ->
    ?ROOM_SERVER_MODULE:send_chat(RoomId, From, Text).

-spec monitor(tinajiupai:room_id()) -> {ok, reference()} | {error, {room_not_found, tianjiupai:room_id()}}.
monitor(RoomId) ->
    ?ROOM_SERVER_MODULE:monitor(RoomId).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_room_id() -> binary().
generate_room_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
