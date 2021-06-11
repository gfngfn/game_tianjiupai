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

-define(ROOM_FRONT, 'Tianjiupai.Room').
-define(USER_FRONT, 'Tianjiupai.User').

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
            {specific_room, _}             -> [<<"PATCH">>];
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
                method       = <<"PATCH">>,
                endpoint     = {specific_room, RoomId},
                session_info = MaybeInfo
            } ->
                handle_room_update(Req0, MaybeInfo, RoomId);
            _ ->
                {false, Req0}
        end,
    {IsSuccess, Req, State}.

-spec provide_json(cowboy_req:req(), #state{}) -> {binary(), cowboy_req:req(), #state{}}.
provide_json(Req0, State) ->
    RespBody =
        case State of
            #state{method = <<"GET">>, endpoint = all_rooms} ->
                RoomStateMaps = ?ROOM_FRONT:get_all_rooms(),
                tianjiupai_format:encode_get_all_rooms_response(RoomStateMaps);
            #state{
                method       = <<"GET">>,
                endpoint     = {specific_room_and_user, RoomId, UserId},
                session_info = MaybeInfo
            } ->
                case validate_cookie(MaybeInfo, UserId) of
                    true ->
                        case ?ROOM_FRONT:get_personal_state(RoomId, UserId) of
                            {ok, PersonalStateMap} ->
                                tianjiupai_format:encode_get_personal_room_response(PersonalStateMap);
                            error ->
                                tianjiupai_format:encode_failure_response(failed_to_get_whole_state)
                                %% TODO: error
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
            case tianjiupai_format:decode_create_user_request(ReqBody) of
                {ok, UserName} ->
                    case ?USER_FRONT:create(UserName) of
                        {ok, UserId} ->
                            Req2 = tianjiupai_session:set(#{user_id => UserId}, Req1),
                            RespBody = tianjiupai_format:encode_create_user_response(UserId),
                            Req3 = cowboy_req:set_resp_body(RespBody, Req2),
                            {true, Req3};
                        {error, Reason} ->
                            Req2 = set_failure_reason_to_resp_body(Reason, Req1),
                            {false, Req2}
                    end;
                {error, Reason} ->
                    Req2 = set_failure_reason_to_resp_body(Reason, Req1),
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
    case tianjiupai_format:decode_create_room_request(ReqBody) of
        {ok, {UserId, RoomName}} ->
            case validate_cookie(MaybeInfo, UserId) of
                true ->
                    case ?ROOM_FRONT:create(RoomName) of
                        {ok, RoomId} ->
                            RespBody = tianjiupai_format:encode_create_room_response(RoomId),
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
        {error, Reason} ->
            Req2 = set_failure_reason_to_resp_body(Reason, Req1),
            {false, Req2}
    end.

%% @doc `PATCH /rooms/<RoomId>'
-spec handle_room_update(
    Req       :: cowboy_req:req(),
    MaybeInfo :: undefined | tinajiupai_session:info(),
    RoomId    :: tianjiupai:room_id()
) ->
    {boolean(), cowboy_req:req()}.
handle_room_update(Req0, MaybeInfo, RoomId) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    case tianjiupai_format:decode_room_request(ReqBody) of
        {ok, RoomRequest} ->
            case RoomRequest of
                {enter_room, UserId} ->
                    case validate_cookie(MaybeInfo, UserId) of
                        true ->
                            handle_enter_room(Req1, RoomId, UserId);
                        false ->
                            Req2 = set_failure_reason_to_resp_body(invalid_cookie, Req1),
                            {false, Req2}
                    end;
                {submit, UserId, Cards} ->
                    case validate_cookie(MaybeInfo, UserId) of
                        true ->
                            handle_submission(Req1, RoomId, UserId, Cards);
                        false ->
                            Req2 = set_failure_reason_to_resp_body(invalid_cookie, Req1),
                            {false, Req2}
                    end
            end;
        {error, Reason} ->
            Req2 = set_failure_reason_to_resp_body(Reason, Req1),
            {false, Req2}
    end.

-spec handle_enter_room(
    Req1      :: cowboy_req:req(),
    RoomId    :: tianjiupai:room_id(),
    UserId    :: tianjiupai:user_id()
) ->
    {boolean(), cowboy_req:req()}.
handle_enter_room(Req1, RoomId, UserId) ->
    case ?USER_FRONT:set_room(UserId, RoomId) of
        {ok, ok} ->
            {ok, UserName} = ?USER_FRONT:get_name(UserId),
            Result = ?ROOM_FRONT:attend(RoomId, UserId, UserName),
            io:format("attend (result: ~p)~n", [Result]),
            case Result of
                {ok, PersonalStateMap} ->
                    RespBody = tianjiupai_format:encode_enter_room_response(PersonalStateMap),
                    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
                    {true, Req2};
                error ->
                    Req2 = set_failure_reason_to_resp_body(attend_failed, Req1),
                    {false, Req2}
            end;
        error ->
            Req2 = set_failure_reason_to_resp_body(set_room_failed, Req1),
            {false, Req2}
    end.

-spec handle_submission(
    Req1      :: cowboy_req:req(),
    RoomId    :: tianjiupai:room_id(),
    UserId    :: tianjiupai:user_id(),
    Cards     :: [tianjiupai:card()]
) ->
    {boolean(), cowboy_req:req()}.
handle_submission(Req1, RoomId, UserId, Cards) ->
    Result = ?ROOM_FRONT:submit(RoomId, UserId, Cards),
    io:format("submit (result: ~p)~n", [Result]),
    case Result of
        {ok, {ObservableGameState, TrickLastOpt}} ->
            RespBody = tianjiupai_format:encode_submit_cards_response(ObservableGameState, TrickLastOpt),
            Req2 = cowboy_req:set_resp_body(RespBody, Req1),
            {true, Req2};
        error ->
            Req2 = set_failure_reason_to_resp_body(submit_failed, Req1),
            {false, Req2}
    end.

-spec set_failure_reason_to_resp_body(Reason :: term(), cowboy_req:req()) -> cowboy_req:req().
set_failure_reason_to_resp_body(Reason, Req) ->
    ReasonBin = tianjiupai_format:encode_failure_response(Reason),
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
            ?USER_FRONT:exists(UserId);
        _ ->
            false
    end.

-spec make_flags_from_cookie(undefined | tianjiupai_session:info()) -> binary().
make_flags_from_cookie(MaybeInfo) ->
    JsonBin = tianjiupai_format:encode_flags_object(MaybeInfo),
    <<"'", JsonBin/binary, "'">>.
