-module(tianjiupai_room_rest).
-behaviour(cowboy_rest).

%%====================================================================================================
%% `cowboy_rest' Callback API
%%====================================================================================================
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2
]).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    endpoint_kind/0
]).
-export([
    accept_request_body/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type http_method() :: binary().

-type endpoint_kind() ::
    all_rooms
  | specific_room.

-type endpoint() ::
    all_rooms
  | {specific_room, RoomId :: tianjiupai_room:room_id()}.

-record(state, {
    method   :: http_method(),
    endpoint :: endpoint()
}).

%%====================================================================================================
%% `cowboy_rest' Callback Functions
%%====================================================================================================
-spec init(cowboy_req:req(), endpoint_kind()) -> {cowboy_rest, cowboy_req:req(), #state{}}.
init(Req, EndpointKind) ->
    %% Method :: http_method()
    Method = cowboy_req:method(Req),
    %% Endpoint :: endpoint()
    Endpoint =
        case {EndpointKind, cowboy_req:binding(room_id, Req, undefined)} of
            {all_rooms,     undefined} -> all_rooms;
            {specific_room, RoomId}    -> {specific_room, RoomId}
            %% Should the `case_clause' exception happen here,
            %% it is due to this implementation being inconsistent with the dispatch table.
        end,
    {cowboy_rest, Req, #state{
        method   = Method,
        endpoint = Endpoint
    }}.

-spec allowed_methods(cowboy_req:req(), #state{}) -> {[http_method()], cowboy_req:req(), #state{}}.
allowed_methods(Req, State) ->
    #state{
        endpoint = Endpoint
    } = State,
    Methods =
        case Endpoint of
            all_rooms          -> [<<"POST">>];
            {specific_room, _} -> [<<"PUT">>]
        end,
    {Methods, Req, State}.

-spec content_types_accepted(cowboy_req:req(), #state{}) -> {Table, cowboy_req:req(), #state{}} when
    Table      :: [{'*' | binary() | ParsedMime, AcceptCallback :: atom()}],
    ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
    Params     :: [{Key :: binary(), Value :: binary()}].
content_types_accepted(Req, State) ->
    Table = [
        {{<<"application">>, <<"json">>, '*'}, accept_request_body}
    ],
    {Table, Req, State}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec accept_request_body(cowboy_req:req(), #state{}) -> {boolean(), cowboy_req:req(), #state{}}.
accept_request_body(Req0, State) ->
    {IsSuccess, Req1} =
        case State of
            #state{method = <<"POST">>, endpoint = all_rooms} ->
                handle_room_creation(Req0);
            #state{method = <<"PUT">>, endpoint = {specific_room, RoomId}} ->
                handle_attending(Req0, RoomId);
            _ ->
                {false, Req0}
        end,
    {IsSuccess, Req1, State}.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
%% @doc `POST /rooms'
-spec handle_room_creation(cowboy_req:req()) -> {boolean(), cowboy_req:req()}.
handle_room_creation(Req0) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    try
        jsone:decode(ReqBody)
    of
        #{
            <<"room_name">> := RoomName
        } when is_binary(RoomName) ->
            RoomId = tianjiupai_room:create(RoomName),
            RespBody = jsone:encode(#{room_id => RoomId}),
            Req2 = cowboy_req:set_resp_body(RespBody, Req1),
            {true, Req2};
        _ ->
            {false, Req1}
    catch
        Class:Reason ->
            io:format("Failed to decode JSON (class: ~p, reason: ~p, body: ~p)~n", [Class, Reason, ReqBody]),
            {false, Req1}
    end.

-spec handle_attending(cowboy_req:req(), tianjiupai_room:room_id()) -> {boolean(), cowboy_req:req()}.
handle_attending(Req0, RoomId) ->
    case tianjiupai_room:attend(RoomId) of
        error ->
            {false, Req0};
        {ok, PlayerIndex} ->
            Info =
                #{
                    belongs_to   => RoomId,
                    player_index => PlayerIndex
                },
            Req1 = tianjiupai_session:set(Info, Req0),
            {true, Req1}
    end.
