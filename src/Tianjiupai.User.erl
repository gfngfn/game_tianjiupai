-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'delete'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1]).
    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2430UserName) -> begin S2431UserId = 'Tianjiupai.User':'generate_user_id'(), begin S2432Res = 'Tianjiupai.UserServerSup':'start_child'(S2431UserId, S2430UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2431UserId end, S2432Res)) end end.
'exists'(S2434UserId) -> 'Tianjiupai.UserServer':'exists'(S2434UserId).
'delete'(S2436UserId) -> begin S2437ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2436UserId), case S2437ProcOpt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', S2438Proc} -> begin S2439Res = 'Tianjiupai.UserServerSup':'terminate_child'(S2438Proc), sesterl_internal_prim:'return'(true) end end end.
'get_name'(S2441UserId) -> 'Tianjiupai.UserServer':'get_name'(S2441UserId).
'get_info'(S2443UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S2443UserId).
'set_room'(S2445UserId, S2446RoomIdOpt) -> 'Tianjiupai.UserServer':'set_room'(S2445UserId, S2446RoomIdOpt).
'get_room'(S2448UserId) -> 'Tianjiupai.UserServer':'get_room'(S2448UserId).
