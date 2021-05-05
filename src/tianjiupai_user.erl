-module(tianjiupai_user).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    create/1,
    get_name/1,
    set_room/2
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

-spec get_name(tianjiupai:user_id()) -> {ok, binary()} | {error, Reason :: term()}.
get_name(UserId) ->
    tianjiupai_user_server:get_name(UserId).

-spec set_room(tianjiupai:user_id(), tianjiupai:room_id()) -> ok | {error, Reason :: term()}.
set_room(UserId, RoomId) ->
    tianjiupai_user_server:set_room(UserId, RoomId).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec generate_user_id() -> binary().
generate_user_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).
