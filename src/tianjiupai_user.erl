-module(tianjiupai_user).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    notify/2
]).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec notify(
    To   :: tianjiupai:user_id(),
    Logs :: [tianjiupai:notifications()]
) ->
    ok.
notify(UserId, Notifications) ->
    case tianjiupai_websocket:notify(UserId, Notifications) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("~p, notify_log failed (reason: ~p, to: ~p, notifications: ~p)",
                [?MODULE, Reason, UserId, Notifications]),
            ok
    end.
