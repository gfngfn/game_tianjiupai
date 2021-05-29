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

-define(ROOM_FRONT, 'Tianjiupai.Room').

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec create(RoomName :: binary()) -> {ok, tianjiupai:room_id()} | {error, Reason :: term()}.
create(RoomName) ->
    ?ROOM_FRONT:create(RoomName).

-spec get_all_rooms() -> [#whole_room_state{}].
get_all_rooms() ->
    Maps = ?ROOM_FRONT:get_all_rooms(),
    lists:map(fun(Map) -> recordify_whole_room_state(Map) end, Maps).

-spec get_whole_state(tianjiupai:room_id()) -> {ok, #whole_room_state{}} | {error, Reason :: term()}.
get_whole_state(RoomId) ->
    case ?ROOM_FRONT:get_whole_state(RoomId) of
        {ok, Map} -> {ok, recordify_whole_room_state(Map)};
        error     -> {error, failed_to_get_whole_state}
    end.

-spec get_personal_state(tianjiupai:room_id(), tianjiupai:user_id()) -> {ok, #personal_room_state{}} | {error, Reason :: term()}.
get_personal_state(RoomId, UserId) ->
    case ?ROOM_FRONT:get_personal_state(RoomId, UserId) of
        {ok, Map} -> {ok, recordify_personal_room_state(Map)};
        error     -> {error, failed_to_get_personal_state}
    end.

-spec attend(tianjiupai:room_id(), tianjiupai:user_id()) -> {ok, #personal_room_state{}} | {error, Reason :: term()}.
attend(RoomId, UserId) ->
    Result = ?ROOM_FRONT:attend(RoomId, UserId),
    io:format("attend (result: ~p)~n", [Result]),
    case Result of
        {ok, Map} -> {ok, recordify_personal_room_state(Map)};
        error     -> {error, failed_to_attend}
    end.

-spec exit(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
exit(RoomId, UserId) ->
    case ?ROOM_FRONT:exit(RoomId, UserId) of
        {ok, ok} -> ok;
        error    -> {error, failed_to_exit}
    end.

send_chat(RoomId, From, Text) ->
    case ?ROOM_FRONT:send_chat(RoomId, From, Text) of
        {ok, ok} -> ok;
        error    -> {error, failed_to_send_chat}
    end.

-spec monitor(tinajiupai:room_id()) -> {ok, reference()} | {error, {room_not_found, tianjiupai:room_id()}}.
monitor(RoomId) ->
    case ?ROOM_FRONT:monitor(RoomId) of
        {ok, MonitorRef} -> {ok, MonitorRef};
        error            -> {error, {room_not_found, RoomId}}
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_room_id() -> binary().
generate_room_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).

recordify_whole_room_state(Map) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        members    := Members,
        is_playing := IsPlaying
    } = Map,
    #whole_room_state{
        room_id    = RoomId,
        room_name  = RoomName,
        members    = Members,
        is_playing = IsPlaying
    }.

recordify_personal_room_state(Map) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        logs       := Logs,
        observable := Observable
    } = Map,
    #personal_room_state{
        room_id    = RoomId,
        room_name  = RoomName,
        logs       = Logs,
        observable = recordify_observable_room_state(Observable)
    }.

recordify_observable_room_state({waiting, _} = Waiting) ->
    Waiting;
recordify_observable_room_state({playing, ObservableGameState}) ->
    #{
        meta              := Meta,
        observable_inning := ObservableInning,
        snapshot_id       := SnapshotId
    } = ObservableGameState,
    {playing, #observable_game_state{
        meta              = Meta,
        observable_inning = recordify_observable_inning_state(ObservableInning),
        snapshot_id       = SnapshotId
    }}.

recordify_observable_inning_state(ObservableInning) ->
    #{
        starts_at := StartsAt,
        your_hand := YourHand,
        gains     := Gains,
        table     := Table
    } = ObservableInning,
    #observable_inning_state{
        starts_at = StartsAt,
        your_hand = YourHand,
        gains     = Gains,
        table     = Table
    }.
