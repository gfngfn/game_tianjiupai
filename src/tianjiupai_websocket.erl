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
    notify_game_start/1,
    notify_chat/3
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(state, {
    session_info :: undefined | tianjiupai_session:info()
}).

-type message() ::
    game_start
  | {chat, From :: binary(), Text :: binary()}.

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
    io:format("~p, websocket_init (info: ~p)~n", [?MODULE, MaybeInfo]), % TODO
    case MaybeInfo of
        undefined ->
            {ok, State};
        #{user_id := UserId} ->
            case register_name(UserId) of
                yes ->
                    io:format("~p, succeeded in registration~n", [?MODULE]), % TODO
                    {ok, State};
                no ->
                %% If something bad happens
                    io:format("~p, failed in registration~n", [?MODULE]), % TODO
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
        game_start ->
            Bin = jsone:encode(#{command => <<"start">>}),
            {reply, [{text, Bin}], State};
        {chat, From, Text} ->
            Bin = jsone:encode(#{command => <<"receive_chat">>, from => From, text => Text}),
            {reply, [{text, Bin}], State};
        _ ->
            io:format("~p, unknown message (messge: ~p)~n", [?MODULE, Msg]),
            {ok, State}
    end.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec notify_game_start(tianjiupai:user_id()) -> ok | {error, error_reason()}.
notify_game_start(UserId) ->
    notify(UserId, game_start).

-spec notify_chat(
    To   :: tianjiupai:user_id(),
    From :: binary(),
    Text :: binary()
) ->
    ok | {error, error_reason()}.
notify_chat(UserId, From, Text) ->
    notify(UserId, {chat, From, Text}).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
-spec register_name(tianjiupai:user_id()) -> yes | no.
register_name(UserId) ->
    Self = self(),
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
        end).

name(UserId) ->
    {?MODULE, UserId}.

-spec notify(tianjiupai:user_id(), message()) -> ok | {error, error_reason()}.
notify(UserId, Msg) ->
    try
        _ = global:send(name(UserId), Msg),
        ok
    catch
        _:_ ->
            {error, {failed_to_notify, UserId, Msg}}
    end.

-spec get_user_id(#state{}) -> {ok, tianjiupai:user_id()} | error.
get_user_id(State) ->
    #state{session_info = MaybeInfo} = State,
    case MaybeInfo of
        #{user_id := UserId} -> {ok, UserId};
        undefined            -> error
    end.

-spec handle_command(iodata(), #state{}) -> {ok, #state{}}.
handle_command(Data, State) ->
    try
        jsone:decode(Data)
    of
        #{
            <<"command">> := <<"set_user_id">>,
            <<"user_id">> := UserId
        } when is_binary(UserId) ->
            io:format("~p: receive (user_id: ~p)~n", [?MODULE, UserId]), % TODO
            Info = #{user_id => UserId},
            case register_name(UserId) of
                yes ->
                    io:format("~p, succeeded in registration~n", [?MODULE]), % TODO
                    ok;
                no ->
                %% If something bad happens
                    io:format("~p, failed in registration~n", [?MODULE]), % TODO
                    ok %% TODO: emit an error
            end,
            {ok, State#state{session_info = Info}};
        #{
            <<"command">> := <<"send_chat">>,
            <<"text">>    := Text
        } when is_binary(Text) ->
            case get_user_id(State) of
                {ok, UserId} ->
                    case tianjiupai_user:send_chat(UserId, Text) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            io:format("~p, failed to send a chat comment (user_id: ~p, text: ~p, reason: ~p)~n",
                                [?MODULE, UserId, Text, Reason]),
                            ok
                    end,
                    {ok, State}
            end;
        _ ->
            io:format("~p: receive (text: ~p)~n", [?MODULE, Data]), % TODO
            {ok, State}
    catch
        Class:Reason ->
            io:format("~p: receive (text: ~p, class: ~p, reason: ~p)~n",
                [?MODULE, Data, Class, Reason]), % TODO
            {ok, State}
    end.
