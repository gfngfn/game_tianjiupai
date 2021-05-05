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
-export([
    start_link/2,
    get_name/1,
    set_room/2
]).
-export_type([
    error_reason/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-record(settings, {
    user_id   :: tianjiupai:user_id(),
    user_name :: binary()
}).

-record(state, {
    settings   :: #settings{},
    belongs_to :: none | {value, {tianjiupai:room_id(), reference()}}
}).

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
        settings   = Settings,
        belongs_to = none
    }}.

-spec handle_call
    ({set_room, tianjiupai:room_id()}, {pid(), reference()}, #state{}) -> {reply, SetRoomReply, #state{}} when
        SetRoomReply :: ok | {error, error_reason()};
    (get_name, {pid(), reference()}, #state{}) -> {reply, GetNameReply, #state{}} when
        GetNameReply :: {ok, binary()} | {error, error_reason()}.
handle_call(CallMsg, _From, State0) ->
    #state{
       settings   = #settings{user_id = UserId, user_name = UserName},
       belongs_to = BelongsTo0
    } = State0,
    case CallMsg of
        get_name ->
            {reply, {ok, UserName}, State0};
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
    UserId   :: tianjiupai:user_id(),
    UserName :: binary()
) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(UserId, UserName) ->
    gen_server:start_link(name(UserId), ?MODULE, {UserId, UserName}, []).

-spec get_name(tianjiupai:user_id()) -> {ok, binary()} | {error, error_reason()}.
get_name(UserId) ->
    case get_pid(UserId) of
        undefined ->
            {error, user_not_found};
        Pid ->
            try
                gen_server:call(Pid, get_name)
            catch
                Class:Reason ->
                    {error, {failed_to_call, Class, Reason}}
            end
    end.

-spec set_room(tianjiupai:user_id(), tianjiupai:room_id()) -> ok | {error, error_reason()}.
set_room(UserId, RoomId) ->
    case get_pid(UserId) of
        undefined ->
            {error, user_not_found};
        Pid ->
            try
                gen_server:call(Pid, {set_room, RoomId})
            catch
                Class:Reason ->
                    {error, {failed_to_call, Class, Reason}}
            end
    end.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
name(UserId) ->
    {global, name_main(UserId)}.

name_main(UserId) ->
    {?MODULE, UserId}.

-spec get_pid(tianjiupai:user_id()) -> undefined | pid().
get_pid(UserId) ->
    global:whereis_name(name_main(UserId)).
