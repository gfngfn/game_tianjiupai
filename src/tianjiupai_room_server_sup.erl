-module(tianjiupai_room_server_sup).
-behaviour(supervisor).

%%====================================================================================================
%% `supervisor' Callback API
%%====================================================================================================
-export([
    init/1
]).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    start_link/0,
    start_child/2,
    which_children/0
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-define(SUP_REF, {global, ?MODULE}).

-define(ROOM_SERVER_MODULE, 'Tianjiupai.RoomServer').

%%====================================================================================================
%% `supervisor' Callback Functions
%%====================================================================================================
init({}) ->
    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => 1,
        period    => 5
    },
    ChildSpec = #{
        id    => undefined,
        start => {?ROOM_SERVER_MODULE, start_link, []},
        type  => worker
    },
    {ok, {SupFlags, [ChildSpec]}}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link(?SUP_REF, ?MODULE, {}).

-spec start_child(
    RoomId   :: tianjiupai:room_id(),
    RoomName :: binary()
) ->
    {ok, pid()}
  | {error, Reason :: term()}.
start_child(RoomId, RoomName) ->
    supervisor:start_child(?SUP_REF, [RoomId, RoomName]).

-spec which_children() -> [tianjiupai_room_server:proc()].
which_children() ->
    Children = supervisor:which_children(?SUP_REF),
    lists:filtermap(
        fun({_Id, Pid, _Type, _Modules}) when is_pid(Pid) ->
                {true, Pid};
           (_Child) ->
                false
        end,
        Children).
