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
-export([
    notify/2,
    notify_by_proc/2,
    notify_room_close/1,
    where_is/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(state, {
    session_info :: tianjiupai_session:info()
}).

-type message() ::
    {notifications, [tianjiupai:notification()]}.

-define(FRONT, 'Tianjiupai.Api').
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
    (?LOGGER:info(
        {"websocket_init (user_id: ~s)", 1},
        {MaybeUserId}
    ))(erlang:atom_to_binary(?MODULE), ?LINE),
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
                        {"succeeded in registration (user_id: ~s)", 1},
                        {UserId}
                    ))(erlang:atom_to_binary(?MODULE), ?LINE),
                    {ok, State};
                {error, Reason} ->
                    (?LOGGER:info(
                        {"succeeded in registration (user_id: ~s, reason: ~p)", 2},
                        {UserId, Reason}
                    ))(erlang:atom_to_binary(?MODULE), ?LINE),
                    {stop, Reason}
            end;
        _ ->
            {stop, user_id_mismatch}
    end.

websocket_handle(MsgFromClient, State) ->
    case MsgFromClient of
        {text, Data} ->
            UserId = get_user_id(State),
            ok = ?FRONT:perform_command(UserId, erlang:iolist_to_binary(Data)),
            {ok, State};
        _ ->
            {ok, State}
    end.

-spec websocket_info(message(), #state{}) -> {reply, [cow_ws:frame()], #state{}} | {ok, #state{}}.
websocket_info(Msg, State) ->
    case Msg of
        {notifications, Notifications} ->
            Chunks =
                lists:map(
                    fun(Notification) ->
                        Bin = ?FRONT:encode_notification(Notification),
                        {text, Bin}
                    end,
                    Notifications),
            {reply, Chunks, State};
        room_closed ->
            UserId = get_user_id(State),
            ?FRONT:subscribe_plaza(UserId, self()),
            Bin = ?FRONT:encode_notification(notify_room_close),
            Chunks = [{text, Bin}],
            {reply, Chunks, State};
        _ ->
            (?LOGGER:warning(
                {"unknown message (message: ~p)", 1},
                {Msg}
            ))(erlang:atom_to_binary(?MODULE), ?LINE),
            {ok, State}
    end.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec notify(tianjiupai:user_id(), [tianjiupai:notification()]) -> ok.
notify(UserId, Notifications) ->
    Msg = {notifications, Notifications},
    try
        _ = global:send(name(UserId), Msg),
        ok
    catch
        Class:Reason ->
            (?LOGGER:warning(
                {"failed to notify (user_id: ~s, notifications: ~p, class: ~p, reason: ~p)", 4},
                {UserId, Notifications, Class, Reason}
            ))(erlang:atom_to_binary(?MODULE), ?LINE),
            ok
    end.

-spec notify_by_proc(pid(), [tianjiupai:notification()]) -> ok.
notify_by_proc(WsHandlerPid, Notifications) ->
    Msg = {notifications, Notifications},
    _ = WsHandlerPid ! Msg,
    ok.

-spec notify_room_close(tianjiupai:user_id()) -> ok.
notify_room_close(UserId) ->
    try
        _ = global:send(name(UserId), room_closed),
        ok
    catch
        Class:Reason ->
            (?LOGGER:warning(
                {"failed to notify room close (user_id: ~s, class: ~p, reason: ~p)", 3},
                {UserId, Class, Reason}
            ))(erlang:atom_to_binary(?MODULE), ?LINE),
            ok
    end.

-spec where_is(tianjiupai:user_id()) -> error | {ok, pid()}.
where_is(UserId) ->
    case global:whereis_name(name(UserId)) of
        undefined                   -> error;
        Pid when erlang:is_pid(Pid) -> {ok, Pid}
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
                    ))(erlang:atom_to_binary(?MODULE), ?LINE),
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
            ok = ?FRONT:set_websocket_connection(UserId, Self),
            ok;
        no ->
            {error, failed_to_regster}
    end.

name(UserId) ->
    {?MODULE, UserId}.

-spec get_user_id(#state{}) -> tianjiupai:user_id().
get_user_id(State) ->
    #state{session_info = #{user_id := UserId}} = State,
    UserId.
