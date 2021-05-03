-module(tianjiupai_app).
-behaviour(application).

%%====================================================================================================
%% `application' Callback API
%%====================================================================================================
-export([
    start/2,
    stop/1
]).

%%====================================================================================================
%% `application' Callback Functions
%%====================================================================================================
start(_StartType, _StartArgs) ->
    cowboy_session:start(),
    IndexFileSpec = {file, "public/index.html", [{mimetypes, {<<"text">>, <<"html">>, []}}]},
    Dispatch =
        cowboy_router:compile([
            {'_', [
                {<<"/">>,               cowboy_static, IndexFileSpec},
                {<<"/assets/[...]">>,   cowboy_static, {dir, "public/assets"}},
                {<<"/users">>,          tianjiupai_room_rest, all_users},
                {<<"/rooms">>,          tianjiupai_room_rest, all_rooms},
                {<<"/rooms/:room_id">>, tianjiupai_room_rest, specific_room}
            ]}
        ]),
    {ok, _} =
        cowboy:start_clear(
            tianjiupai_listener,
            [{port, 8080}],
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
    tianjiupai_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(tianjiupai_listener),
    ok.
