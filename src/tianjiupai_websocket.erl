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
    session_info :: undefined | tianjiupai_session:info()
}).

-type message() ::
    {logs, [tianjiupai:log()]}.

-type error_reason() ::
    {failed_to_notify, tianjiupai:user_id(), message()}.

%%====================================================================================================
%% `cowboy_websocket' Callback Functions
%%====================================================================================================
init(Req0, _) ->
    io:format("~p, init~n", [?MODULE]), % TODO
    {MaybeInfo, Req1} = tianjiupai_session:get(Req0),
    State = #state{session_info = MaybeInfo},
    {cowboy_websocket, Req1, State}.

websocket_init(State) ->
    #state{session_info = MaybeInfo} = State,
    io:format("~p, websocket_init (info: ~p)~n", [?MODULE, MaybeInfo]),
    case MaybeInfo of
        undefined ->
            {ok, State};
        #{user_id := UserId} ->
            case register_name(UserId) of
                ok ->
                    io:format("~p, succeeded in registration~n", [?MODULE]),
                    {ok, State};
                {error, Reason} ->
                    io:format("~p, failed in registration (reason: ~p)~n", [?MODULE, Reason]),
                    {ok, State} %% TODO: emit an error
            end
    end.

websocket_handle(MsgFromClient, State) ->
    case MsgFromClient of
        {text, Data} -> handle_command(Data, State);
        _            -> {ok, State}
    end.

-spec websocket_info(message(), #state{}) -> {reply, [cow_ws:frame()], #state{}} | {ok, #state{}}.
websocket_info(Msg, State) ->
    case Msg of
        {logs, Logs} ->
            Chunks =
                lists:map(
                    fun(Log) ->
                        Bin = tianjiupai_format:encode_notify_log(Log),
                        {text, Bin}
                    end,
                    Logs),
            {reply, Chunks, State};
        _ ->
            io:format("~p, unknown message (messge: ~p)~n", [?MODULE, Msg]),
            {ok, State}
    end.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec notify(tianjiupai:user_id(), [tianjiupai:log()]) -> ok | {error, error_reason()}.
notify(UserId, Logs) ->
    try
        _ = global:send(name(UserId), {logs, Logs}),
        ok
    catch
        _:_ ->
            {error, {failed_to_notify, UserId, Logs}}
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
            fun(_Name, Pid1, Pid2) ->
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
            tianjiupai_user:set_websocket_connection(UserId, Self);
        no ->
            {error, failed_to_regster}
    end.

name(UserId) ->
    {?MODULE, UserId}.

-spec get_user_id(#state{}) -> {ok, tianjiupai:user_id()} | error.
get_user_id(State) ->
    #state{session_info = MaybeInfo} = State,
    case MaybeInfo of
        #{user_id := UserId} -> {ok, UserId};
        undefined            -> error
    end.

-spec handle_command(iodata(), #state{}) -> {ok, #state{}}.
handle_command(Data, State) ->
    case tianjiupai_format:decode_command(Data) of
        {ok, Command} ->
            case Command of
                {set_user_id, UserId} ->
                    io:format("~p: receive (data: ~p)~n", [?MODULE, Data]),
                    Info = #{user_id => UserId},
                    case register_name(UserId) of
                        ok ->
                            io:format("~p: succeeded in registration (pid: ~p)~n", [?MODULE, self()]),
                            ok;
                        {error, Reason} ->
                        %% If something bad happens
                            io:format("~p: failed in registration (pid: ~p, reason: ~p)~n", [?MODULE, self(), Reason]),
                            ok %% TODO: emit an error
                    end,
                    {ok, State#state{session_info = Info}};
                {comment, Text} ->
                    case get_user_id(State) of
                        {ok, UserId} ->
                            case tianjiupai_user:send_chat(UserId, Text) of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    io:format("~p: failed to send a chat comment (user_id: ~p, text: ~p, reason: ~p)~n",
                                        [?MODULE, UserId, Text, Reason]),
                                    ok
                            end,
                            {ok, State};
                        error ->
                            io:format("~p: user not found~n", [?MODULE]),
                            {ok, State}
                    end
            end;
        {error, Reason} ->
            io:format("~p: unknown command (reason: ~p)~n", [?MODULE, Reason]),
            {ok, State}
    end.
