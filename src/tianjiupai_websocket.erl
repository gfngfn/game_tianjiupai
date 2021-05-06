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
    notify_game_start/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(state, {
    session_info :: undefined | tianjiupai_session:info()
}).

-type message() ::
    game_start.

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
                %% If `UserId' has already have a connection
                    io:format("~p, failed in registration~n", [?MODULE]), % TODO
                    {ok, State} %% TODO: emit an error
            end
    end.

websocket_handle(Data, State) ->
    case Data of
        {text, Text} ->
            try
                jsone:decode(Text)
            of
                #{
                    <<"command">> := <<"set_user_id">>,
                    <<"user_id">> := UserId
                } when is_binary(UserId) ->
                    io:format("~p: receive (user_id: ~p)~n", [?MODULE, UserId]), % TODO
                    Info = #{user_id => UserId},
                    {ok, State#state{session_info = Info}};
                _ ->
                    io:format("~p: receive (text: ~p)~n", [?MODULE, Text]), % TODO
                    {ok, State}
            catch
                _:_ ->
                    io:format("~p: receive (text: ~p)~n", [?MODULE, Text]), % TODO
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

-spec websocket_info(message(), #state{}) -> {reply, [cow_ws:frame()], #state{}}.
websocket_info(Msg, State) ->
    case Msg of
        game_start ->
            Bin = jsone:encode(#{command => <<"start">>}),
            {reply, [{text, Bin}], State}
    end.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
notify_game_start(UserId) ->
    notify(UserId, game_start).

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
