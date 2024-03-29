-module(tianjiupai_session).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    info/0
]).
-export([
    set/2,
    get/1,
    expire/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type info() :: #{
    user_id := tianjiupai:user_id()
}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec set(info(), cowboy_req:req()) -> cowboy_req:req().
set(Info, Req0) ->
    {ok, Req1} = cowboy_session:set(info, Info, Req0),
    Req1.

-spec get(cowboy_req:req()) -> {undefined | info(), cowboy_req:req()}.
get(Req0) ->
    cowboy_session:get(info, Req0).

-spec expire(cowboy_req:req()) -> cowboy_req:req().
expire(Req0) ->
    {ok, Req1} = cowboy_session:expire(Req0),
    Req1.
