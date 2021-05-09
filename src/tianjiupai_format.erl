-module(tianjiupai_format).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    command/0
]).
-export([
    %% HTTP:
    encode_flags_object/1,
    decode_create_user_request/1,
    encode_create_user_response/1,
    decode_create_room_request/1,
    encode_create_room_response/1,
    decode_enter_room_request/1,
    encode_enter_room_response/1,
    encode_get_room_response/1,
    encode_get_all_rooms_response/1,
    encode_failure_response/1,

    %% WebSocket:
    decode_command/1,
    encode_notify_log/1,
    encode_notify_game_start/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type command() ::
    {set_user_id, tianjiupai:user_id()}
  | {comment, binary()}.

-define(LABEL_ONLY(_Label_), #{'_label' => _Label_}).
-define(LABELED(_Label_, _Arg_), #{'_label' => _Label_, '_arg' => _Arg_}).

-define(LABELED_PATTERN(_Label_, _Pat_), #{<<"_label">> := _Label_, <<"_arg">> := _Pat_}).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec encode_flags_object(undefined | tianjiupai_session:info()) -> binary().
encode_flags_object(MaybeInfo) ->
    FlagsObj = make_flags_object(MaybeInfo),
    jsone:encode(FlagsObj).

-spec decode_create_user_request(iodata()) ->
    {ok, UserName :: binary()}
  | {error, Reason :: term()}.
decode_create_user_request(ReqBody) ->
    try
        jsone:decode(ReqBody)
    of
        #{
            <<"user_name">> := UserName
        } when is_binary(UserName) ->
            {ok, UserName};
        _ ->
            {error, {invalid_request_body, ReqBody}}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

-spec encode_create_user_response(tianjiupai:user_id()) -> binary().
encode_create_user_response(UserId) ->
    jsone:encode(#{user_id => UserId}).

-spec decode_create_room_request(iodata()) ->
    {ok, {tianjiupai:user_id(), RoomName :: binary()}}
  | {error, Reason :: term()}.
decode_create_room_request(ReqBody) ->
    try
        jsone:decode(ReqBody)
    of
        #{
            <<"user_id">>   := UserId,
            <<"room_name">> := RoomName
        } when is_binary(RoomName) andalso is_binary(UserId) ->
            {ok, {UserId, RoomName}};
        _ ->
            {error, {invalid_request_body, ReqBody}}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

-spec encode_create_room_response(tianjiupai:room_id()) -> binary().
encode_create_room_response(RoomId) ->
    jsone:encode(#{room_id => RoomId}).

-spec decode_enter_room_request(iodata()) ->
    {ok, tianjiupai:user_id()}
  | {error, Reason :: term()}.
decode_enter_room_request(ReqBody) ->
    try
        jsone:decode(ReqBody)
    of
        #{
          <<"user_id">> := UserId
        } when is_binary(UserId) ->
            {ok, UserId};
        _ ->
            {error, {invalid_request_body, ReqBody}}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

-spec encode_enter_room_response(tianjiupai_room:room_state()) -> binary().
encode_enter_room_response(PersonalState) ->
    encode_personal_state(PersonalState).

-spec encode_get_room_response(tianjiupai_room:room_state()) -> binary().
encode_get_room_response(PersonalState) ->
    encode_personal_state(PersonalState).

-spec encode_get_all_rooms_response([tianjiupai_room:room_state()]) -> binary().
encode_get_all_rooms_response(RoomSummaries) ->
    RoomSummaryObjs = lists:map(fun make_room_summary_object/1, RoomSummaries),
    jsone:encode(#{rooms => RoomSummaryObjs}).

-spec encode_personal_state(tianjiupai_room:room_state()) -> binary().
encode_personal_state(RoomState) ->
    PersonalStateObj = make_personal_state_object(RoomState),
    jsone:encode(PersonalStateObj).

-spec encode_failure_response(Reason :: term()) -> binary().
encode_failure_response(Reason) ->
    erlang:list_to_binary(lists:flatten(io_lib:format("~w", [Reason]))).

-spec decode_command(iodata()) -> {ok, command()} | {error, Reason :: term()}.
decode_command(Data) ->
    try
        jsone:decode(Data)
    of
        ?LABELED_PATTERN(<<"CommandSetUserId">>, UserId) when is_binary(UserId) ->
            {ok, {set_user_id, UserId}};
        ?LABELED_PATTERN(<<"CommandComment">>, Text) when is_binary(Text) ->
            {ok, {comment, Text}};
        _ ->
            {error, {invalid_command, Data}}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

-spec encode_notify_log(tianjiupai_room:log()) -> binary().
encode_notify_log(Log) ->
    NotifyLogObj = make_notify_log_object(Log),
    jsone:encode(NotifyLogObj).

-spec encode_notify_game_start() -> binary().
encode_notify_game_start() ->
    NotifyGameStartObj = make_notify_game_start_object(),
    jsone:encode(NotifyGameStartObj).

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
    LogObj = make_log_object(Log),
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
        logs => lists:map(fun make_log_object/1, Logs),
        game => ?LABELED(<<"WaitingStart">>, Members)
    }.
