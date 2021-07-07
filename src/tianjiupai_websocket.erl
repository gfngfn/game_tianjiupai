-module(tianjiupai_websocket).
-behaviour(cowboy_websocket).

%%====================================================================================================
%% `cowboy_websocket' Callback API
%%====================================================================================================
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    error_reason/0
]).
-export([
    notify/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(state, {
    session_info :: tianjiupai_session:info()
}).

-type message() ::
    {notifications, [tianjiupai:notification()]}.

-type error_reason() ::
    {failed_to_notify, tianjiupai:user_id(), message()}.

-define(USER_FRONT, 'Tianjiupai.User').
-define(LOGGER, 'Tianjiupai.Logger').
-define(IDLE_TIMEOUT_MILLISECONDS, 60000).

%%====================================================================================================
%% `cowboy_websocket' Callback Functions
%%====================================================================================================
init(Req0, _) ->
    {MaybeInfo, Req1} = tianjiupai_session:get(Req0),
    MaybeUserId = cowboy_req:binding(user_id, Req1, undefined),
    {cowboy_websocket, Req1, {MaybeUserId, MaybeInfo}, #{idle_timeout => ?IDLE_TIMEOUT_MILLISECONDS}}.

websocket_init({MaybeUserId, MaybeInfo}) ->
    (?LOGGER:info({"websocket_init (user_id: ~p)", 1}, {MaybeUserId}))(?MODULE, ?LINE),
    case {MaybeUserId, MaybeInfo} of
        {undefined, _} ->
            {stop, user_id_unavailable};
        {_, undefined} ->
            {stop, session_unavailable};
        {UserId, #{user_id := UserId} = Info} ->
            State = #state{session_info = Info},
            case register_name(UserId) of
                ok ->
                    (?LOGGER:info(
                        {"succeeded in registration (user_id: ~p)", 1},
                        {UserId}
                    ))(?MODULE, ?LINE),
                    {ok, State};
                {error, Reason} ->
                    (?LOGGER:info(
                        {"succeeded in registration (user_id: ~p, reason: ~p)", 2},
                        {UserId, Reason}
                    ))(?MODULE, ?LINE),
                    {stop, Reason}
            end;
        _ ->
            {stop, user_id_mismatch}
    end.

websocket_handle(MsgFromClient, State) ->
    case MsgFromClient of
        {text, Data} -> handle_command(Data, State);
        _            -> {ok, State}
    end.

-spec websocket_info(message(), #state{}) -> {reply, [cow_ws:frame()], #state{}} | {ok, #state{}}.
websocket_info(Msg, State) ->
    case Msg of
        {notifications, Notifications} ->
            Chunks =
                lists:map(
                    fun(Notification) ->
                        Bin = tianjiupai_format:encode_notification(Notification),
                        {text, Bin}
                    end,
                    Notifications),
            {reply, Chunks, State};
        _ ->
            (?LOGGER:warning({"unknown message (messge: ~p)", 1}, {Msg}))(?MODULE, ?LINE),
            {ok, State}
    end.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec notify(tianjiupai:user_id(), [tianjiupai:notification()]) -> ok | {error, error_reason()}.
notify(UserId, Notifications) ->
    Message = {notifications, Notifications},
    try
        _ = global:send(name(UserId), Message),
        ok
    catch
        _:_ ->
            {error, {failed_to_notify, UserId, Message}}
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec register_name(tianjiupai:user_id()) -> ok | {error, Reason :: term()}.
register_name(UserId) ->
    Self = self(),
    case
        global:register_name(
            name(UserId),
            Self,
            fun(Name, Pid1, Pid2) ->
                    (?LOGGER:warning(
                        {"name clash (name: ~p, pid1: ~p, pid2: ~p, new: ~p)", 4},
                        {Name, Pid1, Pid2, Self}
                    ))(?MODULE, ?LINE),
                    case {Pid1, Pid2} of
                        {Self, _} ->
                            erlang:exit(Pid2),
                            Self;
                        {_, Self} ->
                            erlang:exit(Pid1),
                            Self;
                        _ ->
                            none
                    end
            end)
    of
        yes ->
            case ?USER_FRONT:set_websocket_connection(UserId, Self) of
                {ok, ok} -> ok;
                error    -> {error, set_websocket_connection_failed}
            end;
        no ->
            {error, failed_to_regster}
    end.

name(UserId) ->
    {?MODULE, UserId}.

-spec get_user_id(#state{}) -> tianjiupai:user_id().
get_user_id(State) ->
    #state{session_info = #{user_id := UserId}} = State,
    UserId.

-spec handle_command(iodata(), #state{}) -> {ok, #state{}}.
handle_command(Data, State) ->
    UserId = get_user_id(State),
    case tianjiupai_format:decode_command(Data) of
        {ok, Command} ->
            case Command of
                {comment, Text} ->
                    case ?USER_FRONT:send_chat(UserId, Text) of
                        {ok, ok} ->
                            ok;
                        error ->
                            (?LOGGER:warning(
                                {"failed to send a chat comment (user_id: ~p, text: ~p)", 2},
                                {UserId, Text}
                            ))(?MODULE, ?LINE),
                            ok
                    end,
                    {ok, State};
                {ack, SnapshotId} ->
                    (?LOGGER:info(
                        {"ack (user_id: ~p, snapshot_id: ~p)", 2},
                        {UserId, SnapshotId}
                    ))(?MODULE, ?LINE),
                    ok = ?USER_FRONT:ack(UserId, SnapshotId),
                    {ok, State};
                {next_inning, SnapshotId} ->
                    (?LOGGER:info(
                        {"next inning (user_id: ~p, snapshot_id: ~p)", 2},
                        {UserId, SnapshotId}
                    ))(?MODULE, ?LINE),
                    ok = ?USER_FRONT:require_next_inning(UserId, SnapshotId),
                    {ok, State};
                heartbeat ->
                    {ok, State}
            end;
        {error, Reason} ->
            (?LOGGER:warning(
                {"unknown command (reason: ~p)", 1},
                {Reason}
            ))(?MODULE, ?LINE),
            {ok, State}
    end.
