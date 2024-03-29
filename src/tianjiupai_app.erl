-module(tianjiupai_app).
-behaviour(application).

%%====================================================================================================
%% `application' Callback API
%%====================================================================================================
-export([
    start/2,
    stop/1
]).

-define(SUP, 'Tianjiupai.Sup').

-define(COOKIE_NAME, <<"tianjiupai_session">>).
-define(EXPIRE_SECOND, 86400).
-define(TCP_PORT, 8080).

%%====================================================================================================
%% `application' Callback Functions
%%====================================================================================================
start(_StartType, _StartArgs) ->
    ok = application:set_env(cowboy_session, session, ?COOKIE_NAME),
    ok = application:set_env(cowboy_session, expire, ?EXPIRE_SECOND),
    cowboy_session:start(),
    PrivDirStr = code:priv_dir(tianjiupai),
    PrivDirBin = erlang:list_to_binary(PrivDirStr),
    Template = bbmustache:parse_file(<<PrivDirBin/binary, "/index.mustache.html">>),
    Dispatch =
        cowboy_router:compile([
            {'_', [
                {<<"/">>,
                    tianjiupai_rest, {page, Template}},
                {<<"/assets/[...]">>,
                    cowboy_static, {dir, PrivDirStr ++ "/public/assets"}},
                {<<"/websocket/:user_id">>,
                    tianjiupai_websocket, undefined},
                {<<"/users">>,
                    tianjiupai_rest, all_users},
                {<<"/users/:user_id">>,
                    tianjiupai_rest, specific_user},
                {<<"/rooms">>,
                    tianjiupai_rest, all_rooms},
                {<<"/rooms/:room_id">>,
                    tianjiupai_rest, specific_room},
                {<<"/rooms/:room_id/users/:user_id">>,
                    tianjiupai_rest, specific_room_and_user}
            ]}
        ]),
    {ok, _} =
        cowboy:start_clear(
            tianjiupai_listener,
            [{port, ?TCP_PORT}],
            #{
                env => #{
                    dispatch => Dispatch
                },
                middlewares => [
                    cowboy_session,
                    cowboy_router,
                    cowboy_handler
                ]
            }),
    ?SUP:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(tianjiupai_listener),
    ok.
