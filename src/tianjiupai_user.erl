-module(tianjiupai_user).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    info/0
]).
-export([
    create/1,
    exists/1,
    get_name/1,
    get_info/1,
    set_room/2,
    send_chat/2,
    set_websocket_connection/2,
    notify/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type info() :: #{
    user_name  := binary(),
    belongs_to := none | {value, tianjiupai:room_id()}
}.

-define(USER_FRONT, 'Tianjiupai.User').

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec create(UserName :: binary()) -> {ok, tianjiupai:user_id()} | {error, Reason :: term()}.
create(UserName) ->
    ?USER_FRONT:create(UserName).

-spec exists(tianjiupai:user_id()) -> boolean().
exists(UserId) ->
    ?USER_FRONT:exists(UserId).

-spec get_name(tianjiupai:user_id()) -> {ok, binary()} | {error, Reason :: term()}.
get_name(UserId) ->
    case ?USER_FRONT:get_name(UserId) of
        {ok, UserName} -> {ok, UserName};
        error          -> {error, get_name_failed}
    end.

-spec get_info(tianjiupai:user_id()) -> {ok, info()} | {error, Reason :: term()}.
get_info(UserId) ->
    case ?USER_FRONT:get_info(UserId) of
        {ok, #{user_name := UserName, belongs_to := RoomIdOption}} ->
            MaybeRoomId =
                case RoomIdOption of
                    error        -> none;
                    {ok, RoomId} -> {value, RoomId}
                end,
            {ok, #{user_name => UserName, belongs_to => MaybeRoomId}};
        error ->
            {error, get_info}
    end.

-spec set_room(tianjiupai:user_id(), tianjiupai:room_id()) -> ok | {error, Reason :: term()}.
set_room(UserId, RoomId) ->
    case ?USER_FRONT:set_room(UserId, RoomId) of
        error    -> {error, set_room_failed};
        {ok, ok} -> ok
    end.

-spec send_chat(tianjiupai:user_id(), binary()) -> ok | {error, Reason :: term()}.
send_chat(UserId, Text) ->
    case ?USER_FRONT:send_chat(UserId, Text) of
        error    -> {error, {does_not_belong_to_any_room, UserId}};
        {ok, ok} -> ok
    end.

-spec set_websocket_connection(tianjiupai:user_id(), WsPid :: pid()) -> ok | {error, Reason :: term()}.
set_websocket_connection(UserId, WsPid) ->
    case ?USER_FRONT:set_websocket_connection(UserId, WsPid) of
        {ok, ok} -> ok;
        error    -> {error, set_websocket_connection_failed}
    end.

-spec notify(
    To   :: tianjiupai:user_id(),
    Logs :: [tianjiupai:log()]
) ->
    ok.
notify(UserId, Logs) ->
    case tianjiupai_websocket:notify(UserId, Logs) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("~p, notify_log failed (reason: ~p, to: ~p, logs: ~p)",
                [?MODULE, Reason, UserId, Logs]),
            ok
    end.
