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
    {MaybeInfo, Req1} = tianjiupai_session:get(Req0),
    State = #state{session_info = MaybeInfo},
    {cowboy_websocket, Req1, State}.

websocket_init(State) ->
    #state{session_info = MaybeInfo} = State,
    case MaybeInfo of
        undefined ->
            {ok, State};
        #{user_id := UserId} ->
            case register_name(UserId) of
                yes ->
                    {ok, State};
                no ->
                %% If `UserId' has already have a connection
                    {ok, State} %% TODO: emit an error
            end
    end.

websocket_handle(_Data, State) ->
    {ok, State}.

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
    global:register_name(name(UserId), self(), fun(_Name, _Pid1, _Pid2) -> none end).

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
