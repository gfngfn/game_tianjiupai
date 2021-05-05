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

-type card() ::
    {wen, non_neg_integer()}
  | {wu, non_neg_integer()}.

-type submit() ::
    {single, card()}
  | {double, card(), card()}
  | {triple, card(), card(), card()}
  | {quadruple, card(), card(), card(), card()}.

-record(player, {
    hands     :: [card()],
    gained    :: [card()],
    submitted :: submit()
}).

-record(playing_state, {
    user_ids :: {tianjiupai:user_id(), tianjiupai:user_id(), tianjiupai:user_id(), tianjiupai:user_id()},
    parent   :: 0 | 1 | 2 | 3,
    players  :: {#player{}, #player{}, #player{}, #player{}}
}).

-type game_state() ::
    {waiting, #waiting_state{}}
  | {playing, #playing_state{}}.

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
    ({exit, tianjiupai:user_id()},   {pid(), reference()}, #state{}) -> {reply, ExitReply, #state{}} when
        ExitReply :: ok | {error, error_reason()}.
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
            {IsPlaying, Members} =
                case GameState0 of
                    {waiting, #waiting_state{waiting_members = WaitingMembers}} ->
                        {false, lists:map(fun(#waiting_member{user_id = U}) -> U end, WaitingMembers)};
                    {playing, #playing_state{user_ids = {U0, U1, U2, U3}}} ->
                        {true, [U0, U1, U2, U3]}
                end,
            %% RoomState :: room_state()
            RoomState = #{
                room_id    => RoomId,
                room_name  => RoomName,
                is_playing => IsPlaying,
                members    => Members
            },
            {reply, {ok, RoomState}, State0};
        {attend, UserId} ->
            %% GameState1 :: game_state()
            %% Reply :: attend_reply()
            {GameState1, Reply} =
                case GameState0 of
                    {waiting, #waiting_state{waiting_members = WaitingMembers0}} ->
                        WaitingMember =
                            #waiting_member{
                                user_id  = UserId,
                                is_ready = false
                            },
                        {{waiting, #waiting_state{waiting_members = [WaitingMember | WaitingMembers0]}}, ok};
                    {playing, _} ->
                        {GameState0, {error, playing}}
                end,
            {reply, Reply, State0#state{game_state = GameState1}};
        {exit, UserId} ->
            case GameState0 of
                {waiting, #waiting_state{waiting_members = WaitingMembers0}} ->
                    WaitingMembers =
                        lists:filter(
                            fun(#waiting_member{user_id = UserId0}) ->
                                UserId0 =/= UserId
                            end,
                            WaitingMembers0),
                    {{waiting, #waiting_state{waiting_members = WaitingMembers}}, ok};
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
    case get_pid(RoomId) of
        undefined ->
            {error, {room_not_found, RoomId}};
        Pid ->
            try
                gen_server:call(Pid, {attend, UserId})
            catch
                Class:Reason ->
                    {error, {failed_to_call, Class, Reason}}
            end
    end.

-spec exit(tianjiupai:room_id(), tianjiupai:user_id()) -> ok | {error, error_reason()}.
exit(RoomId, UserId) ->
    case get_pid(RoomId) of
        undefined ->
            {error, {room_not_found, RoomId}};
        Pid ->
            try
                gen_server:call(Pid, {exit, UserId})
            catch
                Class:Reason ->
                    {error, {failed_to_call, Class, Reason}}
            end
    end.

-spec monitor(tianjiupai:room_id()) -> {ok, reference()} | {error, {room_not_found, tianjiupai:room_id()}}.
monitor(RoomId) ->
    case get_pid(RoomId) of
        undefined -> {error, {room_not_found, RoomId}};
        Pid       -> {ok, erlang:monitor(process, Pid)}
    end.

-spec get_state_by_proc(proc()) -> room_state().
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
name(RoomId) ->
    {global, name_main(RoomId)}.

name_main(RoomId) ->
    {?MODULE, RoomId}.

-spec get_pid(tianjiupai:room_id()) -> undefined | pid().
get_pid(RoomId) ->
    global:whereis_name(name_main(RoomId)).
