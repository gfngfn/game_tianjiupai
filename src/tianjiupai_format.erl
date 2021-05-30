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
    encode_get_personal_room_response/1,
    encode_get_all_rooms_response/1,
    encode_failure_response/1,

    %% WebSocket:
    decode_command/1,
    encode_notification/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type command() ::
    {set_user_id, tianjiupai:user_id()}
  | {comment, binary()}.

-type encodable() :: term().

-define(LABEL_ONLY(_Label_), #{'_label' => _Label_}).
-define(LABELED(_Label_, _Arg_), #{'_label' => _Label_, '_arg' => _Arg_}).

-define(LABELED_PATTERN(_Label_, _Pat_), #{<<"_label">> := _Label_, <<"_arg">> := _Pat_}).

-define(USER_FRONT, 'Tianjiupai.User').

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

-spec encode_enter_room_response(tianjiupai:personal_room_state()) -> binary().
encode_enter_room_response(PersonalStateMap) ->
    encode_personal_state(PersonalStateMap).

-spec encode_get_personal_room_response(tianjiupai:personal_room_state()) -> binary().
encode_get_personal_room_response(PersonalStateMap) ->
    encode_personal_state(PersonalStateMap).

-spec encode_get_all_rooms_response([tianjiupai:whole_room_state()]) -> binary().
encode_get_all_rooms_response(WholeStateMaps) ->
    WholeStateObjs = lists:map(fun make_room_summary_object/1, WholeStateMaps),
    jsone:encode(#{rooms => WholeStateObjs}).

-spec encode_personal_state(tianjiupai:personal_room_state()) -> binary().
encode_personal_state(PersonalStateMap) ->
    PersonalStateObj = make_personal_state_object(PersonalStateMap),
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

-spec encode_notification(tianjiupai:notification()) -> binary().
encode_notification(Notification) ->
    NotificationObj = make_notification_object(Notification),
    jsone:encode(NotificationObj).

make_flags_object(MaybeInfo) ->
    case MaybeInfo of
        undefined ->
            #{user => ?LABEL_ONLY(<<"None">>)};
        #{user_id := UserId} ->
            case ?USER_FRONT:get_info(UserId) of
                {ok, Info} ->
                    #{user_name := UserName, belongs_to := MaybeRoomId} = Info,
                    MaybeRoomObj =
                        case MaybeRoomId of
                            error ->
                                ?LABEL_ONLY(<<"None">>);
                            {ok, RoomId} ->
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
                error ->
                    #{user => ?LABEL_ONLY(<<"None">>)}
            end
    end.

-spec make_log_object(tianjiupai:log()) -> encodable().
make_log_object(Log) ->
    case Log of
        {log_comment, From, Text} ->
            ?LABELED(<<"LogComment">>, #{from => From, text => Text});
        {log_entered, UserId} ->
            ?LABELED(<<"LogEntered">>, UserId);
        {log_exited, UserId} ->
            ?LABELED(<<"LogExited">>, UserId);
        log_game_start ->
            ?LABEL_ONLY(<<"LogGameStart">>)
    end.

-spec make_notification_object(tianjiupai:notification()) -> encodable().
make_notification_object(Notification) ->
    case Notification of
        {notify_comment, From, Text} ->
            ?LABELED(<<"NotifyComment">>, #{from => From, text => Text});
        {notify_entered, UserId} ->
            ?LABELED(<<"NotifyEntered">>, UserId);
        {notify_exited, UserId} ->
            ?LABELED(<<"NotifyExited">>, UserId);
        {notify_game_start, ObservableGameState} ->
            ObservableGameStateObj = make_observable_game_state_object(ObservableGameState),
            ?LABELED(<<"NotifyGameStart">>, ObservableGameStateObj);
        notify_next_step ->
            ?LABEL_ONLY(<<"NotifyNextStep">>);
        {notify_submission, SnapshotId, Seat, Cards} ->
            SeatObj = make_seat_object(Seat),
            CardObjs = lists:map(fun make_card_object/1, Cards),
            ?LABELED(<<"NotifySubmission">>, #{snapshot_id => SnapshotId, seat => SeatObj, cards => CardObjs})
    end.

-spec make_room_object(tianjiupai:room_id(), binary()) -> encodable().
make_room_object(RoomId, RoomName) ->
    #{
        room_id    => RoomId,
        room_name  => RoomName
    }.

-spec make_room_summary_object(tianjiupai:whole_room_state()) -> encodable().
make_room_summary_object(WholeStateMap) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        members    := Members,
        is_playing := IsPlaying
    } = WholeStateMap,
    #{
        room       => make_room_object(RoomId, RoomName),
        is_playing => IsPlaying,
        members    => Members
    }.

-spec make_personal_state_object(tianjiupai:personal_room_state()) -> encodable().
make_personal_state_object(PersonalStateMap) ->
    #{
        room_id    := RoomId,
        room_name  := RoomName,
        logs       := Logs,
        observable := Observable
    } = PersonalStateMap,
    case Observable of
        {waiting, Members} ->
            #{
                room => make_room_object(RoomId, RoomName),
                logs => lists:map(fun make_log_object/1, Logs),
                game => ?LABELED(<<"WaitingStart">>, Members)
            };
        {playing, ObservableGameState} ->
            ObservableGameStateObj = make_observable_game_state_object(ObservableGameState),
            #{
                room => make_room_object(RoomId, RoomName),
                logs => lists:map(fun make_log_object/1, Logs),
                game => ?LABELED(<<"PlayingGame">>, ObservableGameStateObj)
            }
    end.

-spec make_observable_game_state_object(tianjiupai:observable_game_state()) -> encodable().
make_observable_game_state_object(ObservableGameStateMap) ->
    #{
        meta              := GameMeta,
        observable_inning := ObservableInning,
        synchronizing     := Synchronizing,
        snapshot_id       := SnapshotId
    } = ObservableGameStateMap,
    GameMetaObj = make_game_meta_object(GameMeta),
    ObservableInningObj = make_observable_inning_object(ObservableInning),
    #{
        meta              => GameMetaObj,
        observable_inning => ObservableInningObj,
        synchronizing     => Synchronizing,
        snapshot_id       => SnapshotId
    }.

-spec make_observable_inning_object(tianjiupai:observable_inning()) -> encodable().
make_observable_inning_object(ObservableInning) ->
    case ObservableInning of
        {observable_during_inning, ObservableInningStateMap} ->
            ?LABELED(<<"ObservableDuringInning">>,
                make_observable_inning_state_object(ObservableInningStateMap));
        {observable_inning_end, GainsQuad} ->
            ?LABELED(<<"ObservableInningEnd">>,
                make_quad_object(fun make_gained_object/1, GainsQuad))
    end.

-spec make_observable_inning_state_object(tianjiupai:observable_inning_state()) -> encodable().
make_observable_inning_state_object(ObservableInningStateMap) ->
    #{
        starts_at := StartsAt,
        your_hand := YourHand,
        gains     := GainsQuad,
        table     := Table
    } = ObservableInningStateMap,
    #{
        starts_at => StartsAt,
        your_hand => lists:map(fun make_card_object/1, YourHand),
        gains     => make_quad_object(fun make_gained_object/1, GainsQuad),
        table     => make_table_object(Table)
    }.

-spec make_table_object(tianjiupai:table_state()) -> encodable().
make_table_object(Table) ->
    case Table of
        starting ->
            ?LABEL_ONLY(<<"Starting">>);
        {wuzun, Exposed} ->
            ?LABELED(<<"TrickWuzun">>, make_exposed_object(fun make_ok_object/1, Exposed));
        {wenzun, Exposed} ->
            ?LABELED(<<"TrickWenzun">>, make_exposed_object(fun make_bool_object/1, Exposed));
        {single_wen, WenExposed} ->
            ?LABELED(<<"TrickSingleWen">>, make_exposed_object(fun make_card_wen_object/1, WenExposed));
        {single_wu, WuExposed} ->
            ?LABELED(<<"TrickSingleWu">>, make_exposed_object(fun make_card_wu_object/1, WuExposed));
        {double_wen, WenExposed} ->
            ?LABELED(<<"TrickDoubleWen">>, make_exposed_object(fun make_card_wen_object/1, WenExposed));
        {double_wu, WuExposed} ->
            ?LABELED(<<"TrickDoubleWu">>, make_exposed_object(fun make_card_wu_object/1, WuExposed));
        {double_both, BigExposed} ->
            ?LABELED(<<"TrickDoubleBoth">>, make_exposed_object(fun make_card_big_object/1, BigExposed));
        {triple_wen, BigExposed} ->
            ?LABELED(<<"TrickTripleWen">>, make_exposed_object(fun make_card_big_object/1, BigExposed));
        {triple_wu, BigExposed} ->
            ?LABELED(<<"TrickTripleWu">>, make_exposed_object(fun make_card_big_object/1, BigExposed));
        {quadruple, BigExposed} ->
            ?LABELED(<<"TrickQuadruple">>, make_exposed_object(fun make_card_big_object/1, BigExposed))
    end.

%% `Big : Tianjiupai.Card.big'
make_card_big_object(Big) ->
    case Big of
        big1 -> 1;
        big2 -> 2;
        big3 -> 3;
        big4 -> 4
    end.

%% `Wen : Tianjiupai.Card.wen'
make_card_wen_object(Wen) ->
    Wen.

%% `Wu : Tianjiupai.Card.wu'
make_card_wu_object(Wu) ->
    Wu.

-spec make_ok_object(ok) -> encodable().
make_ok_object(ok) ->
    ?LABEL_ONLY(<<"Unit">>).

-spec make_bool_object(boolean()) -> encodable().
make_bool_object(true)  -> true;
make_bool_object(false) -> false.

%% `F : fun($a) -> json'
%% `Exposed : exposed($a)'
make_exposed_object(F, Exposed) ->
    {X, XOrCloseds} = Exposed,
    #{
        first      => F(X),
        subsequent => make_closed_or_objects(F, XOrCloseds)
    }.

%% `F : fun($a) -> json'
%% `XOrCloseds : list<closed_or<$a>>'
make_closed_or_objects(F, XOrCloseds) ->
    lists:map(
        fun({open, X}) -> ?LABELED(<<"Open">>, F(X));
           (closed)    -> ?LABEL_ONLY(<<"Closed">>)
        end,
        XOrCloseds).

-spec make_card_object(tianjiupai:card()) -> encodable().
make_card_object(Card) ->
    case Card of
        {wen, Wen} -> ?LABELED(<<"Wen">>, make_card_wen_object(Wen));
        {wu, Wu}   -> ?LABELED(<<"Wu">>, make_card_wu_object(Wu))
    end.

-spec make_gained_object([tianjiupai:card()]) -> encodable().
make_gained_object(Gained) ->
    lists:map(fun make_card_object/1, Gained).

-spec make_game_player_object(tianjiupai:game_player()) -> encodable().
make_game_player_object(GamePlayer) ->
    #{
        user_id := UserId,
        score   := Score
    } = GamePlayer,
    #{
        user_id => UserId,
        score   => Score
    }.

-spec make_game_meta_object(tianjiupai:game_meta()) -> encodable().
make_game_meta_object(GameMeta) ->
    #{
        inning_index     := InningIndex,
        num_consecutives := NumConsecutives,
        parent_seat      := ParentSeat,
        players          := GamePlayerQuad
    } = GameMeta,
    #{
        inning_index     => InningIndex,
        num_consecutives => NumConsecutives,
        parent_seat      => make_seat_object(ParentSeat),
        players          => make_quad_object(fun make_game_player_object/1, GamePlayerQuad)
    }.

-spec make_quad_object(F, tianjiupai:quad(X)) -> encodable() when
    F :: fun((X) -> encodable()),
    X :: term().
make_quad_object(F, Quad) ->
    {X0, X1, X2, X3} = Quad,
    #{
        east  => F(X0),
        south => F(X1),
        west  => F(X2),
        north => F(X3)
    }.

-spec make_seat_object(tianjiupai:seat()) -> encodable().
make_seat_object(seat0) -> 0;
make_seat_object(seat1) -> 1;
make_seat_object(seat2) -> 2;
make_seat_object(seat3) -> 3.
