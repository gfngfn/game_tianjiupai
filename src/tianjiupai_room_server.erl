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

-type internal_room_state() ::
    {waiting, #waiting_state{}}
  | {playing, tianjiupai_game:playing_state()}.

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
            handle_exit(UserId, State0)
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
                                PlayingState =
                                    tianjiupai_game:generate_initial_inning_state(
                                        0,
                                        {UserId0, UserId1, UserId2, UserId}
                                    ),
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
        {playing, Play} ->
            UserIds = tianjiupai_game:get_user_ids(Play),
            {true, UserIds}
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
