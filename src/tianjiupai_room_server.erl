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
    get_state_by_proc/1
]).
-export_type([
    proc/0,
    room_state/0,
    error_reason/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type proc() :: pid().

-type room_state() :: #{
    room_id    := tianjiupai:room_id(),
    room_name  := binary(),
    is_playing := boolean(),
    members    := [tianjiupai:user_id()]
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

-type game_state() ::
    {waiting, #waiting_state{}}
  | {playing, tianjiupai_game:playing_state()}.

-record(state, {
    settings   :: #settings{},
    game_state :: game_state()
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
        settings   = Settings,
        game_state = {waiting, #waiting_state{waiting_members = []}}
    }}.

-spec handle_call
    (get_state, {pid(), reference()}, #state{}) -> {reply, GetStateReply, #state{}} when
        GetStateReply :: {ok, room_state()};
    ({attend, tianjiupai:user_id()}, {pid(), reference()}, #state{}) -> {reply, AttendReply, #state{}} when
        AttendReply :: ok | {error, error_reason()};
    ({exit, tianjiupai:user_id()}, {pid(), reference()}, #state{}) -> {reply, ExitReply, #state{}} when
        ExitReply :: ok | {error, error_reason()};
    ({send_chat, tianjiupai:user_id(), binary()}, {pid(), reference()}, #state{}) -> {reply, ok, #state{}}.
handle_call(CallMsg, _From, State0) ->
    #state{
        settings =
            #settings{
                room_id   = RoomId,
                room_name = RoomName
            },
        game_state =
            GameState0
    } = State0,
    case CallMsg of
        get_state ->
            %% IsPlaying :: boolean()
            %% Members :: [tianjiupai:user_id()]
            {IsPlaying, Members} = get_members_from_state(GameState0),
            %% RoomState :: room_state()
            RoomState = #{
                room_id    => RoomId,
                room_name  => RoomName,
                is_playing => IsPlaying,
                members    => Members
            },
            {reply, {ok, RoomState}, State0};
        {send_chat, From, Text} ->
            {_IsPlaying, Members} = get_members_from_state(GameState0),
            lists:foreach(
                fun(UserId) ->
                    tianjiupai_user:notify_chat(UserId, From, Text)
                end,
                Members),
            {reply, ok, State0};
        {attend, UserId} ->
            %% GameState1 :: game_state()
            %% Reply :: attend_reply()
            {GameState1, Reply} =
                case GameState0 of
                    {waiting, #waiting_state{waiting_members = WaitingMembers0}} ->
                        case
                            lists:any(
                                fun(#waiting_member{user_id = UserId0}) ->
                                    UserId0 =:= UserId
                                end,
                                WaitingMembers0)
                        of
                            true ->
                                {GameState0, ok};
                            false ->
                                WaitingMember =
                                    #waiting_member{
                                        user_id  = UserId,
                                        is_ready = false
                                    },
                                WaitingMembers1 = [WaitingMember | WaitingMembers0],
                                {{waiting, #waiting_state{waiting_members = WaitingMembers1}}, ok}
                        end;
                    {playing, _} ->
                        {GameState0, {error, playing}}
                end,
            {reply, Reply, State0#state{game_state = GameState1}};
        {exit, UserId} ->
            case GameState0 of
                {waiting, #waiting_state{waiting_members = WaitingMembers0}} ->
                    WaitingMembers1 =
                        lists:filter(
                            fun(#waiting_member{user_id = UserId0}) ->
                                UserId0 =/= UserId
                            end,
                            WaitingMembers0),
                    {{waiting, #waiting_state{waiting_members = WaitingMembers1}}, ok};
                {playing, _} ->
                    {GameState0, {error, playing}}
            end
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

-spec attend(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, error_reason()}.
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

-spec get_state_by_proc(proc()) -> {ok, room_state()} | {error, error_reason()}.
get_state_by_proc(RoomServerProc) ->
    try
        gen_server:call(RoomServerProc, get_state)
    catch
        Class:Reason ->
            {error, {failed_to_call, Class, Reason}}
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec get_members_from_state(game_state()) -> {boolean(), [tianjiupai:user_id()]}.
get_members_from_state(GameState) ->
    case GameState of
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
