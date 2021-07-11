-module(tianjiupai_format).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    encode_flags_object/1,
    encode_failure_response/1,
    encode_notification/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type flags() :: #{
    user_id    := tianjiupai:user_id(),
    user_name  := binary(),
    belongs_to := {ok, tianjiupai:room_id()} | error
}.

-type encodable() :: term().

-define(LABEL_ONLY(_Label_), #{'_label' => _Label_}).
-define(LABELED(_Label_, _Arg_), #{'_label' => _Label_, '_arg' => _Arg_}).

-define(LABELED_PATTERN(_Label_, _Pat_), #{<<"_label">> := _Label_, <<"_arg">> := _Pat_}).
-define(LABEL_ONLY_PATTERN(_Label_), #{<<"_label">> := _Label_}).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec encode_flags_object({ok, flags()} | error) -> binary().
encode_flags_object(MaybeFlags) ->
    FlagUserObj = make_flag_user_object(MaybeFlags),
    jsone:encode(FlagUserObj).

-spec make_trick_last_opt_object({ok, tianjiupai:table_state()} | error) -> encodable().
make_trick_last_opt_object(TrickLastOpt) ->
    case TrickLastOpt of
        {ok, TrickLast} -> ?LABELED(<<"Some">>, make_table_object(TrickLast));
        error           -> ?LABEL_ONLY(<<"None">>)
    end.

-spec encode_failure_response(Reason :: term()) -> binary().
encode_failure_response(Reason) ->
    erlang:list_to_binary(lists:flatten(io_lib:format("~w", [Reason]))).

-spec encode_notification(tianjiupai:notification()) -> binary().
encode_notification(Notification) ->
    NotificationObj = make_notification_object(Notification),
    jsone:encode(NotificationObj).

-spec make_flag_user_object({ok, flags()} | error) -> encodable().
make_flag_user_object(MaybeFlags) ->
    case MaybeFlags of
        error ->
            ?LABEL_ONLY(<<"None">>);
        {ok, Flags} ->
            #{user_id := UserId, user_name := UserName, belongs_to := MaybeRoomId} = Flags,
            MaybeRoomObj =
                case MaybeRoomId of
                    error ->
                        ?LABEL_ONLY(<<"None">>);
                    {ok, RoomId} ->
                        ?LABELED(<<"Some">>, RoomId)
                end,
            ?LABELED(<<"Some">>, #{
                id         => UserId,
                name       => UserName,
                belongs_to => MaybeRoomObj
            })
    end.

-spec make_notification_object(tianjiupai:notification()) -> encodable().
make_notification_object(Notification) ->
    case Notification of
        {notify_comment, #{from := From, text := Text}} ->
            ?LABELED(<<"NotifyComment">>, #{from => make_user_object(From), text => Text});
        {notify_entered, User} ->
            ?LABELED(<<"NotifyEntered">>, make_user_object(User));
        {notify_exited, User} ->
            ?LABELED(<<"NotifyExited">>, make_user_object(User));
        {notify_game_start, ObservableGameState} ->
            ObservableGameStateObj = make_observable_game_state_object(ObservableGameState),
            ?LABELED(<<"NotifyGameStart">>, ObservableGameStateObj);
        notify_next_step ->
            ?LABEL_ONLY(<<"NotifyNextStep">>);
        {notify_submission, Submission} ->
            #{
                seat       := Seat,
                submitted  := CardOpts,
                new_state  := ObservableGameState,
                trick_last := TrickLastOpt
            } = Submission,
            SeatObj = make_seat_object(Seat),
            CardOptObjs = lists:map(fun make_card_opt_object/1, CardOpts),
            ObservableGameStateObj = make_observable_game_state_object(ObservableGameState),
            ?LABELED(<<"NotifySubmission">>, #{
                seat       => SeatObj,
                submitted  => CardOptObjs,
                new_state  => ObservableGameStateObj,
                trick_last => make_trick_last_opt_object(TrickLastOpt)
            })
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
        starts_at => make_seat_object(StartsAt),
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
            ?LABELED(<<"Wuzun">>, make_exposed_object(fun make_ok_object/1, Exposed));
        {wenzun, Exposed} ->
            ?LABELED(<<"Wenzun">>, make_exposed_object(fun make_wenzun_object/1, Exposed));
        {single_wen, WenExposed} ->
            ?LABELED(<<"SingleWen">>, make_exposed_object(fun make_card_wen_object/1, WenExposed));
        {single_wu, WuExposed} ->
            ?LABELED(<<"SingleWu">>, make_exposed_object(fun make_card_wu_object/1, WuExposed));
        {double_wen, WenExposed} ->
            ?LABELED(<<"DoubleWen">>, make_exposed_object(fun make_card_wen_object/1, WenExposed));
        {double_wu, WunumExposed} ->
            ?LABELED(<<"DoubleWu">>, make_exposed_object(fun make_card_wu_number_object/1, WunumExposed));
        {double_both, BigdExposed} ->
            ?LABELED(<<"DoubleBoth">>, make_exposed_object(fun make_card_big_with_design_object/1, BigdExposed));
        {triple_wen, BigdExposed} ->
            ?LABELED(<<"TripleWen">>, make_exposed_object(fun make_card_big_with_design_object/1, BigdExposed));
        {triple_wu, BigExposed} ->
            ?LABELED(<<"TripleWu">>, make_exposed_object(fun make_card_big_object/1, BigExposed));
        {quadruple, BigExposed} ->
            ?LABELED(<<"Quadruple">>, make_exposed_object(fun make_card_big_object/1, BigExposed))
    end.

%% `Big : Tianjiupai.Card.big'
make_card_big_object(Big) ->
    case Big of
        big_a -> ?LABEL_ONLY(<<"BigA">>);
        big_b -> ?LABEL_ONLY(<<"BigB">>);
        big_c -> ?LABEL_ONLY(<<"BigC">>);
        big_d -> ?LABEL_ONLY(<<"BigD">>)
    end.

%% `Bigd : Tianjiupai.Types.big_with_design'
make_card_big_with_design_object(Bigd) ->
    #{main := Big, design := B} = Bigd,
    #{main => make_card_big_object(Big), design => B}.

%% `Wen : Tianjiupai.Card.wen'
make_card_wen_object(Wen) ->
    Wen.

%% `Wu : Tianjiupai.Card.wu'
make_card_wu_object(Wu) ->
    Wu.

%% `Wu : Tianjiupai.Card.wu'
make_card_wu_number_object(Wunum) ->
    Wunum.

-spec make_ok_object(wuzun_unit) -> encodable().
make_ok_object(wuzun_unit) ->
    ?LABEL_ONLY(<<"WuzunUnit">>).

-spec make_wenzun_object(wenzun_major | wenzun_minor) -> encodable().
make_wenzun_object(wenzun_major) -> ?LABEL_ONLY(<<"WenzunMajor">>);
make_wenzun_object(wenzun_minor) -> ?LABEL_ONLY(<<"WenzunMinor">>).

%% `F : fun($a) -> json'
%% `Exposed : exposed($a)'
make_exposed_object(F, Exposed) ->
    #{ first := X, subsequent := XOrCloseds} = Exposed,
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

-spec make_card_opt_object({ok, tianjiupai:card()} | error) -> encodable().
make_card_opt_object(CardOpt) ->
    case CardOpt of
        {ok, Card} -> ?LABELED(<<"Some">>, make_card_object(Card));
        error      -> ?LABEL_ONLY(<<"None">>)
    end.

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
        user  := User,
        score := Score
    } = GamePlayer,
    #{
        user  => make_user_object(User),
        score => Score
    }.

-spec make_user_object(tianjiupai:user()) -> encodable().
make_user_object(User) ->
    User.


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
    #{ east := X0, south := X1, west := X2, north := X3 } = Quad,
    #{
        east  => F(X0),
        south => F(X1),
        west  => F(X2),
        north => F(X3)
    }.

-spec make_seat_object(tianjiupai:seat()) -> encodable().
make_seat_object(seat_a) -> ?LABEL_ONLY(<<"SeatA">>);
make_seat_object(seat_b) -> ?LABEL_ONLY(<<"SeatB">>);
make_seat_object(seat_c) -> ?LABEL_ONLY(<<"SeatC">>);
make_seat_object(seat_d) -> ?LABEL_ONLY(<<"SeatD">>).
