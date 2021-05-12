-module(tianjiupai_room_server).
-behaviour(gen_server).

%%====================================================================================================
%% `gen_server' Callback API
%%====================================================================================================
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    start_link/2,
    attend/2,
    exit/2,
    monitor/1,
    send_chat/3,
    get_whole_state/1,
    get_whole_state_by_proc/1
]).
-export_type([
    proc/0,
    log/0,
    snapshot_id/0,
    whole_room_state/0,
    error_reason/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type proc() :: pid().

-type log() ::
    {comment, From :: tianjiupai:user_id(), Text :: binary()}
  | {entered, tianjiupai:user_id()}
  | {exited, tianjiupai:user_id()}
  | game_start.

-type snapshot_id() :: binary().

-type whole_room_state() :: #{
    room_id    := tianjiupai:room_id(),
    room_name  := binary(),
    is_playing := boolean(),
    members    := [tianjiupai:user_id()],
    logs       := [log()]
}.

-record(settings, {
    room_id   :: tianjiupai_room:room_id(),
    room_name :: binary()
}).

-record(waiting_member, {
    user_id  :: tianjiupai:user_id(),
    is_ready :: boolean()
}).

-record(waiting_state, {
    waiting_members :: [#waiting_member{}]
}).

-type score() :: non_neg_integer().

-define(INITIAL_SCORE, 50).

-record(game_player, {
    user_id :: tianjiupai:user_id(),
    score   :: score()
}).

-record(game_state, {
    snapshot_id      :: snapshot_id(),
    inning_index     :: pos_integer(),
    num_consecutives :: pos_integer(),
    parent_seat      :: tianjiupai_quad:seat(),
    players          :: tianjiupai_quad:quad(#game_player{}),
    inning           :: tianjiupai_game:inning_state()
}).

-type internal_room_state() ::
    {waiting, #waiting_state{}}
  | {playing, #game_state{}}.

-record(state, {
    settings      :: #settings{},
    reversed_logs :: [log()],
    room_state    :: internal_room_state()
}).

-type error_reason() ::
    playing
  | {room_not_found, tianjiupai:room_id()}
  | {failed_to_call, Class :: throw | error | exit, Reason :: term()}.

%%====================================================================================================
%% `gen_server' Callback Functions
%%====================================================================================================
init({RoomId, RoomName}) ->
    Settings =
        #settings{
            room_id   = RoomId,
            room_name = RoomName
        },
    {ok, #state{
        settings      = Settings,
        reversed_logs = [],
        room_state    = {waiting, #waiting_state{waiting_members = []}}
    }}.

handle_call(CallMsg, _From, State0) ->
    case CallMsg of
        get_whole_state ->
            RoomState = make_whole_room_state(State0),
            {reply, {ok, RoomState}, State0};
        {send_chat, From, Text} ->
            handle_send_chat(From, Text, State0);
        {attend, UserId} ->
            handle_attend(UserId, State0);
        {exit, UserId} ->
            handle_exit(UserId, State0);
        {progress, SnapshotId, ProgressMsg} ->
            handle_progress(SnapshotId, ProgressMsg, State0)
    end.

handle_cast(CastMsg, State) ->
    io:format("Unexpected cast (message: ~p, state: ~p)~n", [CastMsg, State]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Unexpected info (message: ~p, state: ~p)~n", [Info, State]),
    {noreply, State}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec start_link(
    RoomId   :: tianjiupai_room:room_id(),
    RoomName :: binary()
) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(RoomId, RoomName) ->
    gen_server:start_link(name(RoomId), ?MODULE, {RoomId, RoomName}, []).

-spec attend(tianjiupai:room_id(), tianjiupai:user_id()) -> {ok, whole_room_state()} | {error, error_reason()}.
attend(RoomId, UserId) ->
    call(RoomId, {attend, UserId}).

-spec exit(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, error_reason()}.
exit(RoomId, UserId) ->
    call(RoomId, {exit, UserId}).

-spec send_chat(tianjiupai:room_id(), tianjiupai:user_id(), binary()) -> ok | {error, error_reason()}.
send_chat(RoomId, From, Text) ->
    call(RoomId, {send_chat, From, Text}).

-spec monitor(tianjiupai:room_id()) -> {ok, reference()} | {error, {room_not_found, tianjiupai:room_id()}}.
monitor(RoomId) ->
    case get_pid(RoomId) of
        undefined -> {error, {room_not_found, RoomId}};
        Pid       -> {ok, erlang:monitor(process, Pid)}
    end.

-spec get_whole_state(tianjiupai:room_id()) -> {ok, whole_room_state()} | {error, error_reason()}.
get_whole_state(RoomId) ->
    case get_pid(RoomId) of
        undefined      -> {error, {room_not_found, RoomId}};
        RoomServerProc -> get_whole_state_by_proc(RoomServerProc)
    end.

-spec get_whole_state_by_proc(proc()) -> {ok, whole_room_state()} | {error, error_reason()}.
get_whole_state_by_proc(RoomServerProc) ->
    try
        gen_server:call(RoomServerProc, get_whole_state)
    catch
        Class:Reason ->
            {error, {failed_to_call, Class, Reason}}
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec handle_progress(snapshot_id(), ProgressMsg, #state{}) -> {reply, ProgressReply, #state{}} when
    ProgressMsg ::
        {submit, tianjiupai:user_id(), [tianjiupai_game:card()]},
    ProgressReply ::
        ok
      | {error, Reason},
    Reason ::
        waiting
      | snapshot_mismatch
      | you_are_not_playing
      | tianjiupai_game:submit_error_reason().
handle_progress(SnapshotId, ProgressMsg, State0) ->
    #state{
        room_state = RoomState0
    } = State0,
    %% Result ::
    %%    {ok, {Reply, RoomState1}}
    %%  | {error, Reason}
    Result =
        case RoomState0 of
            {waiting, _} ->
                {error, waiting};
            {playing, GameState0} ->
                #game_state{
                    snapshot_id      = SnapshotId0,
                    inning_index     = _InningIndex0,
                    num_consecutives = _NumConsecutives0,
                    parent_seat      = _ParentSeat0,
                    players          = GamePlayerQuad0,
                    inning           = InningState0
                } = GameState0,
                if
                    SnapshotId =:= SnapshotId0 ->
                        case ProgressMsg of
                            {submit, UserId, SubmittedCards} ->
                                case
                                    tianjiupai_quad:find(
                                        fun(#game_player{user_id = U}) -> U =:= UserId end,
                                        GamePlayerQuad0)
                                of
                                    {ok, {_Submitter, SubmitterSeat}} ->
                                        InningResult = tianjiupai_game:submit(SubmitterSeat, SubmittedCards, InningState0),
                                        case InningResult of
                                            {ok, Next} ->
                                                SnapshotId1 = generate_snapshot_id(),
                                                case Next of
                                                    {continues, InningState1} ->
                                                        %% TODO:
                                                        %% - notify every player that a submission has occurred
                                                        %% - notify the next player "next is your turn"
                                                        %% - update `inning'
                                                        GameState1 =
                                                            GameState0#game_state{
                                                               snapshot_id = SnapshotId1,
                                                               inning      = InningState1
                                                            },
                                                        RoomState1 = {playing, GameState1},
                                                        Reply1 = ok,
                                                        {ok, {Reply1, RoomState1}};
                                                    {wins_trick, _WinnerSeat, _InningState1} ->
                                                        %% TODO:
                                                        %% - notify every player that a submission has occurred
                                                        %% - notify the winner "next is your turn"
                                                        %% - update `inning' (and possibly `score' due to zhizun)
                                                        throw(todo);
                                                    {wins_inning, _WinnerSeat, _GainedsQuad} ->
                                                        %% TODO:
                                                        %% - notify every player that a submission has occurred
                                                        %% - notify the winner "next is your turn"
                                                        %% - update `parent_seat', `score', `inning_index', etc.
                                                        throw(todo)
                                                end;
                                            {error, _} = Err ->
                                                Err
                                        end;
                                    error ->
                                        {error, you_are_not_playing}
                                end
                        end;
                    true ->
                        {error, snapshot_mismatch}
                end
        end,
    case Result of
        {ok, {Reply, RoomState}} ->
            {reply, Reply, State0#state{room_state = RoomState}};
        {error, _} = Err1 ->
            {reply, Err1, State0}
    end.

-spec handle_send_chat(tianjiupai:user_id(), binary(), #state{}) -> {reply, ok, #state{}}.
handle_send_chat(From, Text, State0) ->
    #state{
        reversed_logs = LogAcc0,
        room_state    = RoomState0
    } = State0,
    {_IsPlaying, Members} = get_members_from_state(RoomState0),
    Log = {comment, From, Text},
    notify_logs_for_each(Members, [Log]),
    {reply, ok, State0#state{reversed_logs = [Log | LogAcc0]}}.

-spec handle_exit(tianjiupai:user_id(), #state{}) -> {reply, ExitReply, #state{}} when
    ExitReply :: ok | {error, error_reason()}.
handle_exit(UserId, State0) ->
    #state{
        reversed_logs = LogAcc0,
        room_state    = RoomState0
    } = State0,
    %% RoomState1 :: game_state()
    %% Reply :: ok | {error, error_reason()}
    {RoomState1, LogAcc1, Reply} =
        case RoomState0 of
            {waiting, #waiting_state{waiting_members = WaitingMembers0}} ->
                WaitingMembers1 =
                    lists:filter(
                        fun(#waiting_member{user_id = UserId0}) ->
                            UserId0 =/= UserId
                        end,
                        WaitingMembers0),
                Members = lists:map(fun(#waiting_member{user_id = U}) -> U end, WaitingMembers1),
                %% Log :: log()
                Log = {exited, UserId},
                notify_logs_for_each(Members, [Log]),
                RoomState = {waiting, #waiting_state{waiting_members = WaitingMembers1}},
                {RoomState, [Log | LogAcc0], ok};
            {playing, _} ->
                {RoomState0, LogAcc0, {error, playing}}
        end,
    {reply, Reply, State0#state{room_state = RoomState1, reversed_logs = LogAcc1}}.

-spec handle_attend(tianjiupai:user_id(), #state{}) -> {reply, AttendReply, #state{}} when
    AttendReply :: {ok, whole_room_state()} | {error, error_reason()}.
handle_attend(UserId, State0) ->
    #state{
        reversed_logs = LogAcc0,
        room_state    = RoomState0
    } = State0,
    %% RoomState1 :: game_state()
    %% Result :: ok | {error, error_reason()}
    {RoomState1, LogAcc1, Result} =
        case RoomState0 of
            {playing, _} ->
                {RoomState0, LogAcc0, {error, playing}};
            {waiting, #waiting_state{waiting_members = WaitingMembers0}} ->
                case
                    lists:any(
                        fun(#waiting_member{user_id = UserId0}) ->
                            UserId0 =:= UserId
                        end,
                        WaitingMembers0)
                of
                    true ->
                    %% If the user of `UserId' is already a member
                        {RoomState0, LogAcc0, ok};
                    false ->
                        MembersOtherThanNewOne =
                            lists:map(
                                fun(#waiting_member{user_id = U}) -> U end,
                                WaitingMembers0),
                        LogEnter = {entered, UserId},
                        case MembersOtherThanNewOne of
                            [UserId0, UserId1, UserId2 | _] ->
                            %% If four members will come to be in the room
                                ParentSeat = 0,
                                SnapshotId = generate_snapshot_id(),
                                InningState = tianjiupai_game:generate_initial_inning_state(ParentSeat),
                                PlayerQuad =
                                    tianjiupai_quad:map(
                                        fun(U) ->
                                            #game_player{
                                                user_id = U,
                                                score   = ?INITIAL_SCORE
                                            }
                                        end,
                                        {UserId0, UserId1, UserId2, UserId}),
                                PlayingState =
                                    #game_state{
                                        parent_seat      = ParentSeat,
                                        inning_index     = 1,
                                        num_consecutives = 1,
                                        snapshot_id      = SnapshotId,
                                        players          = PlayerQuad,
                                        inning           = InningState
                                    },
                                RoomState = {playing, PlayingState},
                                LogGameStart = game_start,
                                notify_logs_for_each(MembersOtherThanNewOne, [LogEnter, LogGameStart]),
                                {RoomState, [LogGameStart, LogEnter | LogAcc0], ok};
                            _ ->
                                WaitingMember =
                                    #waiting_member{
                                        user_id  = UserId,
                                        is_ready = false
                                    },
                                WaitingMembers1 = [WaitingMember | WaitingMembers0],
                                RoomState = {waiting, #waiting_state{waiting_members = WaitingMembers1}},
                                notify_logs_for_each(MembersOtherThanNewOne, [LogEnter]),
                                {RoomState, [LogEnter | LogAcc0], ok}
                        end
                end
        end,
    State1 = State0#state{room_state = RoomState1, reversed_logs = LogAcc1},
    Reply =
        case Result of
            ok               -> {ok, make_whole_room_state(State1)};
            {error, _} = Err -> Err
        end,
    {reply, Reply, State1}.

-spec generate_snapshot_id() -> snapshot_id().
generate_snapshot_id() ->
    Uuid = uuid:get_v4(),
    list_to_binary(uuid:uuid_to_string(Uuid)).

-spec notify_logs_for_each([tianjiupai:user_id()], [tianjiupai_user:notification()]) -> ok.
notify_logs_for_each(UserIds, Notifications) ->
    lists:foreach(
        fun(UserId) ->
            tianjiupai_user:notify(UserId, Notifications)
        end,
        UserIds).

-spec make_whole_room_state(#state{}) -> whole_room_state().
make_whole_room_state(State) ->
    #state{
        settings =
            #settings{
                room_id   = RoomId,
                room_name = RoomName
            },
        reversed_logs = LogAcc,
        room_state    = RoomState
    } = State,
    {IsPlaying, Members} = get_members_from_state(RoomState),
    #{
        room_id    => RoomId,
        room_name  => RoomName,
        is_playing => IsPlaying,
        logs       => lists:reverse(LogAcc),
        members    => Members
    }.

-spec get_members_from_state(internal_room_state()) -> {boolean(), [tianjiupai:user_id()]}.
get_members_from_state(RoomState) ->
    case RoomState of
        {waiting, #waiting_state{waiting_members = WaitingMembers}} ->
            {false, lists:map(fun(#waiting_member{user_id = U}) -> U end, WaitingMembers)};
        {playing, #game_state{players = PlayerQuad}} ->
            {U0, U1, U2, U3} = tianjiupai_quad:map(fun(#game_player{user_id = U}) -> U end, PlayerQuad),
            {true, [U0, U1, U2, U3]}
    end.

call(RoomId, Msg) ->
    case get_pid(RoomId) of
        undefined ->
            {error, {room_not_found, RoomId}};
        Pid ->
            try
                gen_server:call(Pid, Msg)
            catch
                Class:Reason ->
                    {error, {failed_to_call, Class, Reason}}
            end
    end.

name(RoomId) ->
    {global, name_main(RoomId)}.

name_main(RoomId) ->
    {?MODULE, RoomId}.

-spec get_pid(tianjiupai:room_id()) -> undefined | pid().
get_pid(RoomId) ->
    global:whereis_name(name_main(RoomId)).
