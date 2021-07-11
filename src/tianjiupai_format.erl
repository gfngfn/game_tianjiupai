-module(tianjiupai_format).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export([
    encode_flags_object/1,
    encode_failure_response/1
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type flags() :: #{
    user_id    := tianjiupai:user_id(),
    user_name  := binary(),
    belongs_to := {ok, tianjiupai:room_id()} | error
}.

-type encodable() :: term().

-define(LABEL_ONLY(_Label_), #{'_label' => _Label_}).
-define(LABELED(_Label_, _Arg_), #{'_label' => _Label_, '_arg' => _Arg_}).

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec encode_flags_object({ok, flags()} | error) -> binary().
encode_flags_object(MaybeFlags) ->
    FlagUserObj = make_flag_user_object(MaybeFlags),
    jsone:encode(FlagUserObj).

-spec encode_failure_response(Reason :: term()) -> binary().
encode_failure_response(Reason) ->
    erlang:list_to_binary(lists:flatten(io_lib:format("~w", [Reason]))).

-spec make_flag_user_object({ok, flags()} | error) -> encodable().
make_flag_user_object(MaybeFlags) ->
    case MaybeFlags of
        error ->
            ?LABEL_ONLY(<<"None">>);
        {ok, Flags} ->
            #{user_id := UserId, user_name := UserName, belongs_to := MaybeRoomId} = Flags,
            MaybeRoomObj =
                case MaybeRoomId of
                    error ->
                        ?LABEL_ONLY(<<"None">>);
                    {ok, RoomId} ->
                        ?LABELED(<<"Some">>, RoomId)
                end,
            ?LABELED(<<"Some">>, #{
                id         => UserId,
                name       => UserName,
                belongs_to => MaybeRoomObj
            })
    end.
