-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'delete'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1]).
    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2639UserName) -> begin S2640UserId = 'Tianjiupai.User':'generate_user_id'(), begin S2641Res = 'Tianjiupai.UserResourceServer':'add'(S2640UserId, S2639UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2640UserId end, S2641Res)) end end.
'exists'(S2643UserId) -> 'Tianjiupai.UserServer':'exists'(S2643UserId).
'delete'(S2645UserId) -> 'Tianjiupai.UserServer':'delete'(S2645UserId).
'get_name'(S2647UserId) -> 'Tianjiupai.UserServer':'get_name'(S2647UserId).
'get_info'(S2649UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S2649UserId).
'set_room'(S2651UserId, S2652RoomIdOpt) -> 'Tianjiupai.UserServer':'set_room'(S2651UserId, S2652RoomIdOpt).
'get_room'(S2654UserId) -> 'Tianjiupai.UserServer':'get_room'(S2654UserId).
