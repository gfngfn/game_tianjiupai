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
    attend/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type room_id() :: binary().

-type player_index() :: non_neg_integer().

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec create(RoomName :: binary()) -> room_id().
create(_RoomName) ->
    %% TODO: registers rooms
    <<"hoge">>.

-spec attend(room_id()) -> {ok, player_index()} | error.
attend(_RoomId) ->
    %% TODO: judge
    error.
