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
-type info() :: tianjiupai_user_server:info().

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec create(UserName :: binary()) -> {ok, tianjiupai:user_id()} | {error, Reason :: term()}.
create(UserName) ->
    UserId = generate_user_id(),
    case tianjiupai_user_server_sup:start_child(UserId, UserName) of
        {ok, _Pid}       -> {ok, UserId};
        {error, _} = Err -> Err
    end.

-spec exists(tianjiupai:user_id()) -> boolean().
exists(UserId) ->
    tianjiupai_user_server:exists(UserId).

-spec get_name(tianjiupai:user_id()) -> {ok, binary()} | {error, Reason :: term()}.
get_name(UserId) ->
    tianjiupai_user_server:get_name(UserId).

-spec get_info(tianjiupai:user_id()) -> {ok, info()} | {error, Reason :: term()}.
get_info(UserId) ->
    tianjiupai_user_server:get_info(UserId).

-spec set_room(tianjiupai:user_id(), tianjiupai:room_id()) -> ok | {error, Reason :: term()}.
set_room(UserId, RoomId) ->
    tianjiupai_user_server:set_room(UserId, RoomId).

-spec send_chat(tianjiupai:user_id(), binary()) -> ok | {error, Reason :: term()}.
send_chat(UserId, Text) ->
    case tianjiupai_user_server:get_room(UserId) of
        {error, _} = Err      -> Err;
        {ok, none}            -> {error, {does_not_belong_to_any_room, UserId}};
        {ok, {value, RoomId}} -> tianjiupai_room:send_chat(RoomId, UserId, Text)
    end.

-spec set_websocket_connection(tianjiupai:user_id(), WsPid :: pid()) -> ok | {error, Reason :: term()}.
set_websocket_connection(UserId, WsPid) ->
    tianjiupai_user_server:set_websocket_connection(UserId, WsPid).

-spec notify(
    To   :: tianjiupai:user_id(),
    Logs :: [tianjiupai_room:log()]
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

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_user_id() -> binary().
generate_user_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
