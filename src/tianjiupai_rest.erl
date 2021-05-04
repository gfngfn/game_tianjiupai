-module(tianjiupai_rest).
-behaviour(cowboy_rest).

%%====================================================================================================
%% `cowboy_rest' Callback API
%%====================================================================================================
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    endpoint_kind/0
]).
-export([
    accept_request_body/2,
    provide_page/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type http_method() :: binary().

-type endpoint_kind() ::
    {page, bbmustache:template()}
  | all_users
  | all_rooms
  | specific_room.

-type endpoint() ::
    all_users
  | all_rooms
  | {specific_room, RoomId :: tianjiupai_room:room_id()}.

-record(state, {
    method       :: http_method(),
    endpoint     :: endpoint(),
    session_info :: undefined | tianjiupai_session:info()
}).

%%====================================================================================================
%% `cowboy_rest' Callback Functions
%%====================================================================================================
-spec init(cowboy_req:req(), endpoint_kind()) -> {cowboy_rest, cowboy_req:req(), #state{}}.
init(Req0, EndpointKind) ->
    %% Method :: http_method()
    Method = cowboy_req:method(Req0),
    %% MaybeInfo :: tianjiupai_session:info()
    {MaybeInfo, Req1} = tianjiupai_session:get(Req0),
    %% Endpoint :: endpoint()
    Endpoint =
        case {EndpointKind, cowboy_req:binding(room_id, Req1, undefined)} of
            {{page, Template}, undefined} -> {page, Template};
            {all_users,        undefined} -> all_users;
            {all_rooms,        undefined} -> all_rooms;
            {specific_room,    RoomId}    -> {specific_room, RoomId}
            %% Should the `case_clause' exception happen here,
            %% it is due to this implementation being inconsistent with the dispatch table.
        end,
    {cowboy_rest, Req1, #state{
        method       = Method,
        endpoint     = Endpoint,
        session_info = MaybeInfo
    }}.

-spec allowed_methods(cowboy_req:req(), #state{}) -> {[http_method()], cowboy_req:req(), #state{}}.
allowed_methods(Req, State) ->
    #state{
        endpoint = Endpoint
    } = State,
    Methods =
        case Endpoint of
            {page, _}          -> [<<"GET">>];
            all_users          -> [<<"POST">>];
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

content_types_provided(Req, State) ->
    #state{endpoint = Endpoint} = State,
    Table =
        case Endpoint of
            {page, _} -> [{{<<"text">>, <<"html">>, '*'}, provide_page}];
            _         -> [{{<<"application">>, <<"json">>, '*'}, undefined}]
        end,
    {Table, Req, State}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec accept_request_body(cowboy_req:req(), #state{}) -> {boolean(), cowboy_req:req(), #state{}}.
accept_request_body(Req0, State) ->
    {IsSuccess, Req} =
        case State of
            #state{method = <<"POST">>, endpoint = all_users, session_info = MaybeInfo} ->
                handle_user_creation(Req0, MaybeInfo);
            #state{method = <<"POST">>, endpoint = all_rooms} ->
                handle_room_creation(Req0);
            #state{method = <<"PUT">>, endpoint = {specific_room, RoomId}} ->
                handle_attending(Req0, RoomId);
            _ ->
                {false, Req0}
        end,
    {IsSuccess, Req, State}.

-spec provide_page(cowboy_req:req(), #state{}) -> {binary(), cowboy_req:req(), #state{}}.
provide_page(Req0, State) ->
    RespBody =
        case State of
            #state{method = <<"GET">>, endpoint = {page, Template}, session_info = MaybeInfo} ->
                handle_page(Template, MaybeInfo);
            _ ->
                <<"">>
        end,
    {RespBody, Req0, State}.

%%====================================================================================================
%% Internal Functions
%%====================================================================================================
%% @doc `GET /'
-spec handle_page(
    Template  :: bbmustache:template(),
    MaybeInfo :: undefined | tianjiupai_session:info()
) ->
    binary().
handle_page(Template, MaybeInfo) ->
    Flags =
        case MaybeInfo of
            undefined ->
                #{user_id => #{type => <<"nothing">>}};
            #{user_id := UserId} ->
                #{user_id => #{type => <<"just">>, value => UserId}}
        end,
    FlagsBin = jsone:encode(Flags),
    bbmustache:compile(Template, #{"flags" => <<"'", FlagsBin/binary, "'">>}, [{escape_fun, fun(Bin) -> Bin end}]).

%% @doc `POST /users'
-spec handle_user_creation(
    Req       :: cowboy_req:req(),
    MaybeInfo :: undefined | tianjiupai_session:info()
) ->
    {boolean(), cowboy_req:req()}.
handle_user_creation(Req0, MaybeInfo) ->
    case MaybeInfo of
        #{user_id := UserId} ->
                RespBody = jsone:encode(#{user_id => UserId}),
                Req1 = cowboy_req:set_resp_body(RespBody, Req0),
                {true, Req1};
        undefined ->
            {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
            try
                jsone:decode(ReqBody)
            of
                #{
                    <<"user_name">> := UserName
                } when is_binary(UserName) ->
                    case tianjiupai_user:create(UserName) of
                        {ok, UserId} ->
                            Req2 = tianjiupai_session:set(#{user_id => UserId}, Req1),
                            RespBody = jsone:encode(#{user_id => UserId}),
                            Req3 = cowboy_req:set_resp_body(RespBody, Req2),
                            {true, Req3};
                        {error, Reason} ->
                            Req2 = set_failure_reason_to_resp_body(Reason, Req1),
                            {false, Req2}
                    end
            catch
                Class:Reason ->
                    Req2 = set_failure_reason_to_resp_body({exception, Class, Reason}, Req1),
                    {false, Req2}
            end
    end.

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
            case tianjiupai_room:create(RoomName) of
                {ok, RoomId} ->
                    RespBody = jsone:encode(#{room_id => RoomId}),
                    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
                    {true, Req2};
                {error, _Reason} ->
                    {false, Req1}
            end;
        _ ->
            {false, Req1}
    catch
        Class:Reason ->
            Req2 = set_failure_reason_to_resp_body({exception, Class, Reason}, Req1),
            {false, Req2}
    end.

%% @doc `PUT /rooms/<RoomId>'
-spec handle_attending(cowboy_req:req(), tianjiupai_room:room_id()) -> {boolean(), cowboy_req:req()}.
handle_attending(Req0, RoomId) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    try
        jsone:decode(ReqBody)
    of
        #{
          <<"user_id">> := UserId
        } when is_binary(UserId) ->
            case tianjiupai_user:set_room(UserId, RoomId) of
                {error, Reason1} ->
                    Req2 = set_failure_reason_to_resp_body(Reason1, Req1),
                    {false, Req2};
                ok ->
                    case tianjiupai_room:attend(RoomId, UserId) of
                        {error, Reason2} ->
                            Req2 = set_failure_reason_to_resp_body(Reason2, Req1),
                            {false, Req2};
                        ok ->
                            {true, Req1}
                    end
            end
    catch
        Class:Reason ->
            Req2 = set_failure_reason_to_resp_body({exception, Class, Reason}, Req1),
            {false, Req2}
    end.

-spec set_failure_reason_to_resp_body(Reason :: term(), cowboy_req:req()) -> cowboy_req:req().
set_failure_reason_to_resp_body(Reason, Req) ->
    ReasonBin = erlang:list_to_binary(lists:flatten(io_lib:format("~w", [Reason]))),
    RespBody = jsone:encode(#{reason => ReasonBin}),
    cowboy_req:set_resp_body(RespBody, Req).
