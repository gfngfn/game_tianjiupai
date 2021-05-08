-module(tianjiupai_format).

-export([
    make_log_object/1,
    make_room_state_object/1
]).

-spec make_log_object(tianjiupai_room:log()) -> term().
make_log_object(Log) ->
    case Log of
        {comment, From, Text} ->
            #{
                type => <<"comment">>,
                from => From,
                text => Text
            };
        {entered, UserId} ->
            #{
                type    => <<"entered">>,
                user_id => UserId
            };
        {exited, UserId} ->
            #{
                type    => <<"exited">>,
                user_id => UserId
            }
    end.

-spec make_room_state_object(tianjiupai_room:room_state()) -> term().
make_room_state_object(RoomState) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        is_playing := IsPlaying,
        members    := Members,
        logs       := Logs
    } = RoomState,
    #{
        room_id    => RoomId,
        room_name  => RoomName,
        is_playing => IsPlaying,
        members    => Members,
        logs       => lists:map(fun ?MODULE:make_log_object/1, Logs)
    }.
