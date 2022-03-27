-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'delete'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1]).
    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2667UserName) -> begin S2668UserId = 'Tianjiupai.User':'generate_user_id'(), begin S2669Res = 'Tianjiupai.UserResourceServer':'add'(S2668UserId, S2667UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2668UserId end, S2669Res)) end end.
'exists'(S2671UserId) -> 'Tianjiupai.UserServer':'exists'(S2671UserId).
'delete'(S2673UserId) -> 'Tianjiupai.UserServer':'delete'(S2673UserId).
'get_name'(S2675UserId) -> 'Tianjiupai.UserServer':'get_name'(S2675UserId).
'get_info'(S2677UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S2677UserId).
'set_room'(S2679UserId, S2680RoomIdOpt) -> 'Tianjiupai.UserServer':'set_room'(S2679UserId, S2680RoomIdOpt).
'get_room'(S2682UserId) -> 'Tianjiupai.UserServer':'get_room'(S2682UserId).
