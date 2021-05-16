-module(tianjiupai_user_server_sup).
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
    start_child/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-define(SUP_REF, {global, ?MODULE}).

-define(USER_SERVER_MODULE, 'Tianjiupai.UserServer').

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
        start => {?USER_SERVER_MODULE, start_link, []},
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
    UserId   :: tianjiupai:user_id(),
    UserName :: binary()
) ->
    {ok, pid()}
  | {error, Reason :: term()}.
start_child(UserId, UserName) ->
    supervisor:start_child(?SUP_REF, [UserId, UserName]).
