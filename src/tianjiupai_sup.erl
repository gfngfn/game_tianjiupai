-module(tianjiupai_sup).
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
    start_link/0
]).

%%====================================================================================================
%% `supervisor' Callback Functions
%%====================================================================================================
init(_) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 1,
        period    => 5
    },
    ChildSpecs = [
        #{
            id    => tianjiupai_room_server_sup,
            start => {tianjiupai_room_server_sup, start_link, []},
            type  => supervisor
        },
        #{
            id    => tianjiupai_user_server_sup,
            start => {tianjiupai_user_server_sup, start_link, []},
            type  => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
