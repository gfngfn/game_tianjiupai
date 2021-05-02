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
    Dispatch =
        cowboy_router:compile([
            {'_', [
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
