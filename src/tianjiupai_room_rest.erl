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
    create_room
  | enter_room.

-type endpoint() ::
    create_room
  | {enter_room, RoomId :: tianjiupai_room:room_id()}.

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
            {create_room, undefined} -> create_room;
            {enter_room,  RoomId}    -> {enter_room, RoomId}
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
            create_room     -> [<<"POST">>];
            {enter_room, _} -> [<<"PUT">>]
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
            #state{method = <<"POST">>, endpoint = create_room} ->
                handle_create_room(Req0);
            _ ->
                {false, Req0}
        end,
    {IsSuccess, Req1, State}.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
%% @doc `POST /rooms'
-spec handle_create_room(cowboy_req:req()) -> {boolean(), cowboy_req:req()}.
handle_create_room(Req0) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    case jsone:decode(ReqBody) of
        #{
            <<"room_name">> := RoomName
        } ->
            RoomId = tianjiupai_room:create(RoomName),
            RespBody = jsone:encode(#{room_id => RoomId}),
            Req1 = cowboy_req:set_resp_body(RespBody, Req0),
            {true, Req1};
        _ ->
            {false, Req0}
    end.
