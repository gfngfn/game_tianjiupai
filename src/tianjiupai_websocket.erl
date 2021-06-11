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
-define(IDLE_TIMEOUT_MILLISECONDS, 60000).

%%====================================================================================================
%% `cowboy_websocket' Callback Functions
%%====================================================================================================
init(Req0, _) ->
    io:format("~p, init~n", [?MODULE]), % TODO
    {MaybeInfo, Req1} = tianjiupai_session:get(Req0),
    MaybeUserId = cowboy_req:binding(user_id, Req1, undefined),
    {cowboy_websocket, Req1, {MaybeUserId, MaybeInfo}, #{idle_timeout => ?IDLE_TIMEOUT_MILLISECONDS}}.

websocket_init({MaybeUserId, MaybeInfo}) ->
    io:format("~p, websocket_init (user_id: ~p, info: ~p)~n", [?MODULE, MaybeUserId, MaybeInfo]),
    case {MaybeUserId, MaybeInfo} of
        {undefined, _} ->
            {stop, user_id_unavailable};
        {_, undefined} ->
            {stop, session_unavailable};
        {UserId, #{user_id := UserId} = Info} ->
            State = #state{session_info = Info},
            case register_name(UserId) of
                ok ->
                    io:format("~p, succeeded in registration~n", [?MODULE]),
                    {ok, State};
                {error, Reason} ->
                    io:format("~p, failed in registration (reason: ~p)~n", [?MODULE, Reason]),
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
            io:format("~p, unknown message (messge: ~p)~n", [?MODULE, Msg]),
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
                    io:format("name clash (name: ~p, pid1: ~p, pid2: ~p, new: ~p)~n", [Name, Pid1, Pid2, Self]),
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
                            io:format("~p: failed to send a chat comment (user_id: ~p, text: ~p)~n",
                                [?MODULE, UserId, Text]),
                            ok
                    end,
                    {ok, State};
                {ack, SnapshotId} ->
                    io:format("~p: ack (user_id: ~p, snapshot_id: ~p)~n",
                        [?MODULE, UserId, SnapshotId]),
                    ok = ?USER_FRONT:ack(UserId, SnapshotId),
                    {ok, State};
                {next_inning, SnapshotId} ->
                    io:format("~p: next inning (user_id: ~p, snapshot_id: ~p)~n",
                        [?MODULE, UserId, SnapshotId]),
                    ok = ?USER_FRONT:require_next_inning(UserId, SnapshotId),
                    {ok, State};
                heartbeat ->
                    io:format("~p: heartbeat (user_id: ~p)~n", [?MODULE, UserId]),
                    {ok, State}
            end;
        {error, Reason} ->
            io:format("~p: unknown command (reason: ~p)~n", [?MODULE, Reason]),
            {ok, State}
    end.
