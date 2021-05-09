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
    accept_json/2,
    provide_json/2,
    provide_html/2
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type http_method() :: binary().

-type endpoint_kind() ::
    {page, bbmustache:template()}
  | all_users
  | all_rooms
  | specific_room
  | specific_room_and_user.

-type endpoint() ::
    {page, bbmustache:template()}
  | all_users
  | all_rooms
  | {specific_room, tianjiupai:room_id()}
  | {specific_room_and_user, tianjiupai:room_id(), tianjiupai:user_id()}.

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
        case EndpointKind of
            {page, Template} ->
                {page, Template};
            all_users ->
                all_users;
            all_rooms ->
                all_rooms;
            specific_room ->
                RoomId = cowboy_req:binding(room_id, Req1, undefined),
                {specific_room, RoomId};
            specific_room_and_user ->
                RoomId = cowboy_req:binding(room_id, Req1, undefined),
                UserId = cowboy_req:binding(user_id, Req1, undefined),
                {specific_room_and_user, RoomId, UserId}
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
            {page, _}                      -> [<<"GET">>];
            all_users                      -> [<<"POST">>];
            all_rooms                      -> [<<"GET">>, <<"POST">>];
            {specific_room, _}             -> [<<"PUT">>];
            {specific_room_and_user, _, _} -> [<<"GET">>]
        end,
    {Methods, Req, State}.

-spec content_types_accepted(cowboy_req:req(), #state{}) -> {Table, cowboy_req:req(), #state{}} when
    Table      :: [{'*' | binary() | ParsedMime, AcceptCallback :: atom()}],
    ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
    Params     :: [{Key :: binary(), Value :: binary()}].
content_types_accepted(Req, State) ->
    Table = [
        {{<<"application">>, <<"json">>, '*'}, accept_json}
    ],
    {Table, Req, State}.

content_types_provided(Req, State) ->
    MimeJson = {<<"application">>, <<"json">>, '*'},
    MimeHtml = {<<"text">>, <<"html">>, '*'},
    #state{method = Method, endpoint = Endpoint} = State,
    Table =
        case Method of
            <<"GET">> ->
                case Endpoint of
                    {page, _}                      -> [{MimeHtml, provide_html}];
                    all_rooms                      -> [{MimeJson, provide_json}];
                    {specific_room_and_user, _, _} -> [{MimeJson, provide_json}];
                    _                              -> []
                end;
            _ ->
                [{MimeJson, undefined}]
        end,
    {Table, Req, State}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec accept_json(cowboy_req:req(), #state{}) -> {boolean(), cowboy_req:req(), #state{}}.
accept_json(Req0, State) ->
    {IsSuccess, Req} =
        case State of
            #state{
                method       = <<"POST">>,
                endpoint     = all_users,
                session_info = MaybeInfo
            } ->
                handle_user_creation(Req0, MaybeInfo);
            #state{
                method       = <<"POST">>,
                endpoint     = all_rooms,
                session_info = MaybeInfo
            } ->
                handle_room_creation(Req0, MaybeInfo);
            #state{
                method       = <<"PUT">>,
                endpoint     = {specific_room, RoomId},
                session_info = MaybeInfo
            } ->
                handle_attending(Req0, MaybeInfo, RoomId);
            _ ->
                {false, Req0}
        end,
    {IsSuccess, Req, State}.

-spec provide_json(cowboy_req:req(), #state{}) -> {binary(), cowboy_req:req(), #state{}}.
provide_json(Req0, State) ->
    RespBody =
        case State of
            #state{method = <<"GET">>, endpoint = all_rooms} ->
                %% Rooms :: [tianjiupai_room:room_state()]
                RoomStates = tianjiupai_room:get_all_rooms(),
                encode_room_summaries(RoomStates);
            #state{
                method       = <<"GET">>,
                endpoint     = {specific_room_and_user, RoomId, UserId},
                session_info = MaybeInfo
            } ->
                case validate_cookie(MaybeInfo, UserId) of
                    true ->
                        case tianjiupai_room:get_room(RoomId) of
                            {ok, RoomState} -> encode_personal_state(RoomState);
                            {error, Reason} -> encode_failure_reason(Reason)
                        end;
                    false ->
                        <<"">> % TODO: error
                end;
            _ ->
                <<"">> % TODO: error
        end,
    {RespBody, Req0, State}.

-spec provide_html(cowboy_req:req(), #state{}) -> {binary(), cowboy_req:req(), #state{}}.
provide_html(Req0, State) ->
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
    FlagsBin = make_flags_from_cookie(MaybeInfo),
    bbmustache:compile(Template, #{"flags" => FlagsBin}, [{escape_fun, fun(Bin) -> Bin end}]).

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
-spec handle_room_creation(
    Req       :: cowboy_req:req(),
    MaybeInfo :: undefined | tianjiupai_session:info()
) ->
    {boolean(), cowboy_req:req()}.
handle_room_creation(Req0, MaybeInfo) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    try
        jsone:decode(ReqBody)
    of
        #{
            <<"user_id">>   := UserId,
            <<"room_name">> := RoomName
        } when is_binary(RoomName) andalso is_binary(UserId) ->
            case validate_cookie(MaybeInfo, UserId) of
                true ->
                    case tianjiupai_room:create(RoomName) of
                        {ok, RoomId} ->
                            RespBody = jsone:encode(#{room_id => RoomId}),
                            Req2 = cowboy_req:set_resp_body(RespBody, Req1),
                            {true, Req2};
                        {error, Reason} ->
                            Req2 = set_failure_reason_to_resp_body(Reason, Req1),
                            {false, Req2}
                    end;
                false ->
                    Req2 = set_failure_reason_to_resp_body(invalid_cookie, Req1),
                    {false, Req2}
            end;
        _ ->
            Req2 = set_failure_reason_to_resp_body(invalid_request_body, Req1),
            {false, Req2}
    catch
        Class:Reason ->
            Req2 = set_failure_reason_to_resp_body({exception, Class, Reason}, Req1),
            {false, Req2}
    end.

%% @doc `PUT /rooms/<RoomId>'
-spec handle_attending(
    Req       :: cowboy_req:req(),
    MaybeInfo :: undefined | tinajiupai_session:info(),
    RoomId    :: tianjiupai_room:room_id()
) ->
    {boolean(), cowboy_req:req()}.
handle_attending(Req0, MaybeInfo, RoomId) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    try
        jsone:decode(ReqBody)
    of
        #{
          <<"user_id">> := UserId
        } when is_binary(UserId) ->
            case validate_cookie(MaybeInfo, UserId) of
                true ->
                    case tianjiupai_user:set_room(UserId, RoomId) of
                        {error, Reason1} ->
                            Req2 = set_failure_reason_to_resp_body(Reason1, Req1),
                            {false, Req2};
                        ok ->
                            case tianjiupai_room:attend(RoomId, UserId) of
                                {error, Reason2} ->
                                    Req2 = set_failure_reason_to_resp_body(Reason2, Req1),
                                    {false, Req2};
                                {ok, RoomState} ->
                                    RespBody = encode_personal_state(RoomState),
                                    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
                                    {true, Req2}
                            end
                    end;
                false ->
                    Req2 = set_failure_reason_to_resp_body(invalid_cookie, Req1),
                    {false, Req2}
            end
    catch
        Class:Reason ->
            Req2 = set_failure_reason_to_resp_body({exception, Class, Reason}, Req1),
            {false, Req2}
    end.

-spec encode_personal_state(tianjiupai_room:room_state()) -> binary().
encode_personal_state(RoomState) ->
    PersonalStateObj = tianjiupai_format:make_personal_state_object(RoomState),
    jsone:encode(PersonalStateObj).

-spec encode_room_summaries([tianjiupai_room:room_state()]) -> binary().
encode_room_summaries(RoomStates) ->
    RoomSummaryObjs = lists:map(fun tianjiupai_format:make_room_summary_object/1, RoomStates),
    jsone:encode(#{rooms => RoomSummaryObjs}).

-spec encode_failure_reason(Reason :: term()) -> binary().
encode_failure_reason(Reason) ->
    erlang:list_to_binary(lists:flatten(io_lib:format("~w", [Reason]))).

-spec set_failure_reason_to_resp_body(Reason :: term(), cowboy_req:req()) -> cowboy_req:req().
set_failure_reason_to_resp_body(Reason, Req) ->
    ReasonBin = encode_failure_reason(Reason),
    RespBody = jsone:encode(#{reason => ReasonBin}),
    cowboy_req:set_resp_body(RespBody, Req).

-spec validate_cookie(
    MaybeInfo :: undefined | tianjiupai_session:info(),
    UserId    :: tianjiupai:user_id()
) ->
    boolean().
validate_cookie(MaybeInfo, UserId) ->
    case MaybeInfo of
        #{user_id := UserId} ->
        %% Note that here `UserId' has already been bound.
        %% That is, this pattern includes equality testing.
            tianjiupai_user:exists(UserId);
        _ ->
            false
    end.

-spec make_flags_from_cookie(undefined | tianjiupai_session:info()) -> binary().
make_flags_from_cookie(MaybeInfo) ->
    FlagsObj = tianjiupai_format:make_flags_object(MaybeInfo),
    JsonBin = jsone:encode(FlagsObj),
    <<"'", JsonBin/binary, "'">>.
