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
    attend/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
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
    parent  :: 0 | 1 | 2 | 3,
    players :: {#player{}, #player{}, #player{}, #player{}}
}).

-type game_state() ::
    {waiting, #waiting_state{}}
  | {playing, #playing_state{}}.

-record(state, {
    settings   :: #settings{},
    game_state :: game_state()
}).

-type attend_reply() ::
    ok
  | {error, full}.

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
    ({attend, tianjiupai:user_id()}, {pid(), reference()}, #state{}) -> {reply, attend_reply(), #state{}}.
handle_call(CallMsg, _From, State0) ->
    #state{game_state = GameState0} = State0,
    case CallMsg of
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
                        {GameState0, {error, full}}
                end,
            {reply, Reply, State0#state{game_state = GameState1}}
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

-spec attend(tianjiupai_room:room_id(), tianjiupai:user_id()) -> attend_reply().
attend(RoomId, UserId) ->
    gen_server:call(name(RoomId), {attend, UserId}).

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
name(RoomId) ->
    {global, {?MODULE, RoomId}}.
