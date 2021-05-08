-module(tianjiupai_user_server).
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
-export_type([
    info/0,
    error_reason/0
]).
-export([
    start_link/2,
    exists/1,
    get_name/1,
    get_room/1,
    get_info/1,
    set_room/2,
    set_websocket_connection/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(settings, {
    user_id   :: tianjiupai:user_id(),
    user_name :: binary()
}).

-record(state, {
    settings             :: #settings{},
    belongs_to           :: none | {value, {tianjiupai:room_id(), reference()}},
    websocket_connection :: reference()
}).

-type info() :: #{
    user_name  := binary(),
    belongs_to := none | {value, tianjiupai:room_id()}
}.

-type error_reason() ::
    tianjiupai_room_server:error_reason()
  | {user_not_found, tianjiupai:user_id()}
  | {failed_to_call, Class :: atom(), Reason :: term()}.

%%====================================================================================================
%% `gen_server' Callback Functions
%%====================================================================================================
init({UserId, UserName}) ->
    Settings =
        #settings{
            user_id   = UserId,
            user_name = UserName
        },
    {ok, #state{
        settings             = Settings,
        belongs_to           = none,
        websocket_connection = none
    }}.

-spec handle_call
    ({set_room, tianjiupai:room_id()}, {pid(), reference()}, #state{}) -> {reply, SetRoomReply, #state{}} when
        SetRoomReply :: ok | {error, error_reason()};
    (get_room, {pid(), reference()}, #state{}) -> {reply, GetRoomReply, #state{}} when
        GetRoomReply :: {ok, none | {value, tianjiupai:room_id()}, #state{}};
    (get_name, {pid(), reference()}, #state{}) -> {reply, GetNameReply, #state{}} when
        GetNameReply :: {ok, binary()} | {error, error_reason()};
    (get_info, {pid(), reference()}, #state{}) -> {reply, GetInfoReply, #state{}} when
        GetInfoReply :: {ok, info()} | {error, error_reason()}.
handle_call(CallMsg, _From, State0) ->
    #state{
       settings             = #settings{user_id = UserId, user_name = UserName},
       belongs_to           = BelongsTo0,
       websocket_connection = WebSocketConnection0
    } = State0,
    case CallMsg of
        get_name ->
            {reply, {ok, UserName}, State0};
        get_room ->
            Content =
                case BelongsTo0 of
                    none                           -> none;
                    {value, {RoomId, _MonitorRef}} -> {value, RoomId}
                end,
            {reply, {ok, Content}, State0};
        get_info ->
            MaybeRoomId =
                case BelongsTo0 of
                    none                           -> none;
                    {value, {RoomId, _MonitorRef}} -> {value, RoomId}
                end,
            Info = #{
                user_name  => UserName,
                belongs_to => MaybeRoomId
            },
            {reply, {ok, Info}, State0};
        {set_room, RoomId} ->
            Result =
                case BelongsTo0 of
                    none ->
                        ok;
                    {value, {RoomId0, RoomMonitorRef0}} ->
                        case tianjiupai_room:exit(RoomId0, UserId) of
                            {error, _} = Err ->
                                Err;
                            ok ->
                                erlang:demonitor(RoomMonitorRef0),
                                ok
                        end
                end,
            case Result of
                {error, _} = Err1 ->
                    {reply, Err1, State0#state{
                        belongs_to = none
                    }};
                ok ->
                    case tianjiupai_room:monitor(RoomId) of
                        {error, _} = Err2 ->
                            {reply, Err2, State0#state{
                                belongs_to = none
                            }};
                        {ok, RoomMonitorRef} ->
                            {reply, ok, State0#state{
                                belongs_to = {value, {RoomId, RoomMonitorRef}}
                            }}
                    end
            end;
        {set_websocket_connection, WsPid} ->
            case WebSocketConnection0 of
                {value, WsMonitorRef0} ->
                    erlang:demonitor(WsMonitorRef0),
                    ok;
                none ->
                    ok
            end,
            WsMonitorRef = erlang:monitor(process, WsPid),
            {reply, ok, State0#state{
                websocket_connection = {value, WsMonitorRef}
            }}
    end.

handle_cast(CastMsg, State) ->
    io:format("Unexpected cast (message: ~p, state: ~p)~n", [CastMsg, State]),
    {noreply, State}.

handle_info(Info, State) ->
    #state{
        settings             = #settings{user_id = UserId},
        belongs_to           = BelongsTo,
        websocket_connection = MaybeWsMonitorRef
    } = State,
    case Info of
        {'DOWN', MonitorRef, process, _Pid, Reason} ->
            case MaybeWsMonitorRef of
                {value, MonitorRef} ->
                    io:format("WebSocket connection closed (user_id: ~p, reason: ~p)~n",
                        [UserId, Reason]),
                    {noreply, State#state{websocket_connection = none}};
                _ ->
                    case BelongsTo of
                        {value, {RoomId, MonitorRef}} ->
                            io:format("Room closed (user_id: ~p, room_id: ~p, reason: ~p)~n",
                                [UserId, RoomId, Reason]),
                            {noreply, State#state{belongs_to = none}};
                        _ ->
                            {noreply, State}
                    end
            end;
        _ ->
            io:format("Unexpected info (message: ~p, state: ~p)~n", [Info, State]),
            {noreply, State}
    end.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec start_link(
    UserId   :: tianjiupai:user_id(),
    UserName :: binary()
) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(UserId, UserName) ->
    gen_server:start_link(name(UserId), ?MODULE, {UserId, UserName}, []).

-spec exists(tianjiupai:user_id()) -> boolean().
exists(UserId) ->
    case get_pid(UserId) of
        undefined -> false;
        _Pid      -> true
    end.

-spec get_name(tianjiupai:user_id()) -> {ok, binary()} | {error, error_reason()}.
get_name(UserId) ->
    call(UserId, get_name).

-spec get_room(tianjiupai:user_id()) -> {ok, none | {value, tianjiupai:room_id()}} | {error, error_reason()}.
get_room(UserId) ->
    call(UserId, get_room).

-spec get_info(tianjiupai:user_id()) -> {ok, info()} | {error, error_reason()}.
get_info(UserId) ->
    call(UserId, get_info).

-spec set_room(tianjiupai:user_id(), tianjiupai:room_id()) -> ok | {error, error_reason()}.
set_room(UserId, RoomId) ->
    call(UserId, {set_room, RoomId}).

set_websocket_connection(UserId, WsPid) ->
    call(UserId, {set_websocket_connection, WsPid}).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
call(UserId, Msg) ->
    case get_pid(UserId) of
        undefined ->
            {error, user_not_found};
        Pid ->
            try
                gen_server:call(Pid, Msg)
            catch
                Class:Reason ->
                    {error, {failed_to_call, Class, Reason}}
            end
    end.

name(UserId) ->
    {global, name_main(UserId)}.

name_main(UserId) ->
    {?MODULE, UserId}.

-spec get_pid(tianjiupai:user_id()) -> undefined | pid().
get_pid(UserId) ->
    global:whereis_name(name_main(UserId)).
