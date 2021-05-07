-module(tianjiupai_user).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    create/1,
    exists/1,
    get_name/1,
    set_room/2,
    send_chat/2,
    notify_chat/3
]).

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

-spec notify_chat(
    To   :: tianjiupai:user_id(),
    From :: tianjiupai:user_id(),
    Text :: binary()
) ->
    ok.
notify_chat(UserId, From, Text) ->
    case tianjiupai_websocket:notify_chat(UserId, From, Text) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("~p, notify_chat failed (reason: ~p, to: ~p, from: ~p, text: ~p)",
                [?MODULE, Reason, UserId, From, Text]),
            ok
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_user_id() -> binary().
generate_user_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
