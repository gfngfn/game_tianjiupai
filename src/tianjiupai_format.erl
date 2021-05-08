-module(tianjiupai_format).

-export([
    make_flags_object/1,
    make_notify_log_object/1,
    make_notify_game_start_object/0,
    make_log_object/1,
    make_room_object/2,
    make_room_summary_object/1,
    make_personal_state_object/1
]).

-define(LABEL_ONLY(_Label_), #{'_label' => _Label_}).
-define(LABELED(_Label_, _Arg_), #{'_label' => _Label_, '_arg' => _Arg_}).

make_flags_object(MaybeInfo) ->
    case MaybeInfo of
        undefined ->
            #{user => ?LABEL_ONLY(<<"None">>)};
        #{user_id := UserId} ->
            case tianjiupai_user:get_info(UserId) of
                {ok, Info} ->
                    #{user_name := UserName, belongs_to := MaybeRoomId} = Info,
                    MaybeRoomObj =
                        case MaybeRoomId of
                            none ->
                                ?LABEL_ONLY(<<"None">>);
                            {value, RoomId} ->
                                ?LABELED(<<"Some">>, RoomId)
                        end,
                    #{
                        user =>
                            ?LABELED(<<"Some">>, #{
                                id         => UserId,
                                name       => UserName,
                                belongs_to => MaybeRoomObj
                            })
                    };
                {error, _} ->
                    #{user => ?LABEL_ONLY(<<"None">>)}
            end
    end.

-spec make_notify_log_object(tianjiupai_room:log()) -> term().
make_notify_log_object(Log) ->
    LogObj = ?MODULE:make_log_object(Log),
    ?LABELED(<<"NotifyLog">>, LogObj).

-spec make_notify_game_start_object() -> term().
make_notify_game_start_object() ->
    ?LABEL_ONLY(<<"NotifyGameStart">>).

-spec make_log_object(tianjiupai_room:log()) -> term().
make_log_object(Log) ->
    case Log of
        {comment, From, Text} ->
                ?LABELED(<<"LogComment">>, #{from => From, text => Text});
        {entered, UserId} ->
                ?LABELED(<<"LogEntered">>, UserId);
        {exited, UserId} ->
                ?LABELED(<<"LogExited">>, UserId)
    end.

-spec make_room_object(tianjiupai:room_id(), binary()) -> term().
make_room_object(RoomId, RoomName) ->
    #{
        room_id    => RoomId,
        room_name  => RoomName
    }.

-spec make_room_summary_object(tianjiupai_room:room_state()) -> term().
make_room_summary_object(RoomState) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        is_playing := IsPlaying,
        members    := Members
    } = RoomState,
    #{
        room       => make_room_object(RoomId, RoomName),
        is_playing => IsPlaying,
        members    => Members
    }.

%% TODO: replace room states with personally observed states
-spec make_personal_state_object(tianjiupai_room:room_state()) -> term().
make_personal_state_object(RoomState) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        logs       := Logs,
        members    := Members
    } = RoomState,
    #{
        room => make_room_object(RoomId, RoomName),
        logs => lists:map(fun ?MODULE:make_log_object/1, Logs),
        game => ?LABELED(<<"WaitingStart">>, Members)
    }.
