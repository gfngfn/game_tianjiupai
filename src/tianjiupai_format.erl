-module(tianjiupai_format).

-include("tianjiupai.hrl").

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
    encode_notify_log/1
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

-spec encode_get_personal_room_response(tianjiupai_room:room_state()) -> binary().
encode_get_personal_room_response(PersonalState) ->
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

-spec make_log_object(tianjiupai_room:log()) -> term().
make_log_object(Log) ->
    case Log of
        {comment, From, Text} ->
            ?LABELED(<<"LogComment">>, #{from => From, text => Text});
        {entered, UserId} ->
            ?LABELED(<<"LogEntered">>, UserId);
        {exited, UserId} ->
            ?LABELED(<<"LogExited">>, UserId);
        game_start ->
            ?LABEL_ONLY(<<"GameStart">>)
    end.

-spec make_room_object(tianjiupai:room_id(), binary()) -> term().
make_room_object(RoomId, RoomName) ->
    #{
        room_id    => RoomId,
        room_name  => RoomName
    }.

-spec make_room_summary_object(tianjiupai_room:whole_room_state()) -> term().
make_room_summary_object(WholeRoomState) ->
    #whole_room_state{
        room_id    = RoomId,
        room_name  = RoomName,
        is_playing = IsPlaying,
        members    = Members
    } = WholeRoomState,
    #{
        room       => make_room_object(RoomId, RoomName),
        is_playing => IsPlaying,
        members    => Members
    }.

%% TODO: replace room states with personally observed states
-spec make_personal_state_object(#personal_room_state{}) -> term().
make_personal_state_object(RoomState) ->
    #personal_room_state{
        room_id    = RoomId,
        room_name  = RoomName,
        logs       = Logs,
        observable = Observable
    } = RoomState,
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

-spec make_observable_game_state_object(#observable_game_state{}) -> term().
make_observable_game_state_object(ObservableGameState) ->
    #observable_game_state{
        meta              = GameMeta,
        observable_inning = ObservableInning,
        snapshot_id       = SnapshotId
    } = ObservableGameState,
    GameMetaObj = make_game_meta_object(GameMeta),
    ObservableInningObj = make_observable_inning_state_object(ObservableInning),
    #{
        meta              => GameMetaObj,
        observable_inning => ObservableInningObj,
        snapshot_id       => SnapshotId
    }.


-spec make_observable_inning_state_object(#observable_inning_state{}) -> term().
make_observable_inning_state_object(ObservableInning) ->
    #observable_inning_state{
        starts_at = StartSeat,
        your_hand = Hand,
        gains     = GainedQuad,
        table     = Table
    } = ObservableInning,
    #{
        starts_at => StartSeat,
        your_hand => lists:map(fun make_card_object/1, Hand),
        gains     => make_quad_object(fun make_gained_object/1, GainedQuad),
        table     => make_table_object(Table)
    }.

-spec make_table_object(tianjiupai_game:table_state()) -> term().
make_table_object(Table) ->
    case Table of
        starting ->
            ?LABEL_ONLY(<<"Starting">>);
        {wuzun, Exposed} ->
            ?LABELED(<<"Wuzun">>, make_exposed_object(fun make_ok_object/1, Exposed));
        {wenzun, Exposed} ->
            ?LABELED(<<"Wenzun">>, make_exposed_object(fun make_bool_object/1, Exposed))
    end.

-spec make_ok_object(ok) -> term().
make_ok_object(ok) ->
    ?LABEL_ONLY(<<"Unit">>).

-spec make_bool_object(boolean()) -> term().
make_bool_object(true)  -> true;
make_bool_object(false) -> false.

-spec make_exposed_object(F, tianjiupai_game:exposed(X)) -> term() when
      F :: fun((X) -> term()),
      X :: term().
make_exposed_object(F, Exposed) ->
    {X, XOrCloseds} = Exposed,
    #{
        first      => F(X),
        subsequent => make_closed_or_objects(F, XOrCloseds)
    }.

-spec make_closed_or_objects(F, [tianjiupai_game:closed_or(X)]) -> term() when
      F :: fun((X) -> term()),
      X :: term().
make_closed_or_objects(F, XOrCloseds) ->
    lists:map(
        fun({open, X}) -> ?LABELED(<<"Open">>, F(X));
           (closed)    -> ?LABEL_ONLY(<<"Closed">>)
        end,
        XOrCloseds).

-spec make_card_object(tianjiupai_game:card()) -> term().
make_card_object(Card) ->
    case Card of
        {wen, Wen} -> ?LABELED(<<"Wen">>, Wen);
        {wu, Wu}   -> ?LABELED(<<"Wu">>, Wu)
    end.

-spec make_gained_object([tianjiupai_game:card()]) -> term().
make_gained_object(Gained) ->
    lists:map(fun make_card_object/1, Gained).

-spec make_game_player_object(#game_player{}) -> term().
make_game_player_object(GamePlayer) ->
    #game_player{
        user_id = UserId,
        score   = Score
    } = GamePlayer,
    #{
        user_id => UserId,
        score   => Score
    }.

-spec make_game_meta_object(#game_meta{}) -> term().
make_game_meta_object(GameMeta) ->
    #game_meta{
        inning_index     = InningIndex,
        num_consecutives = NumConsecutives,
        parent_seat      = ParentSeat,
        players          = GamePlayerQuad
    } = GameMeta,
    #{
        inning_index     => InningIndex,
        num_consecutives => NumConsecutives,
        parent_seat      => ParentSeat,
        players          => make_quad_object(fun make_game_player_object/1, GamePlayerQuad)
    }.

-spec make_quad_object(F, tianjiupai_quad:quad(X)) -> term() when
    F :: fun((X) -> term()),
    X :: term().
make_quad_object(F, Quad) ->
    {X0, X1, X2, X3} = Quad,
    #{
        east  => F(X0),
        south => F(X1),
        west  => F(X2),
        north => F(X3)
    }.
