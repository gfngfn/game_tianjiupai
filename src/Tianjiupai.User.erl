-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'delete'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1]).
    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2659UserName) -> begin S2660UserId = 'Tianjiupai.User':'generate_user_id'(), begin S2661Res = 'Tianjiupai.UserResourceServer':'add'(S2660UserId, S2659UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2660UserId end, S2661Res)) end end.
'exists'(S2663UserId) -> 'Tianjiupai.UserServer':'exists'(S2663UserId).
'delete'(S2665UserId) -> 'Tianjiupai.UserServer':'delete'(S2665UserId).
'get_name'(S2667UserId) -> 'Tianjiupai.UserServer':'get_name'(S2667UserId).
'get_info'(S2669UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S2669UserId).
'set_room'(S2671UserId, S2672RoomIdOpt) -> 'Tianjiupai.UserServer':'set_room'(S2671UserId, S2672RoomIdOpt).
'get_room'(S2674UserId) -> 'Tianjiupai.UserServer':'get_room'(S2674UserId).
