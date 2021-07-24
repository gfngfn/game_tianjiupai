-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'delete'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1, 'send_chat'/2, 'ack'/2, 'require_next_inning'/2, 'set_websocket_connection'/2]).
    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2217UserName) -> begin S2218UserId = 'Tianjiupai.User':'generate_user_id'(), begin S2219Res = 'Tianjiupai.UserServerSup':'start_child'(S2218UserId, S2217UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S2220Proc) -> S2218UserId end, S2219Res)) end end.
'exists'(S2222UserId) -> 'Tianjiupai.UserServer':'exists'(S2222UserId).
'delete'(S2224UserId) -> begin S2225ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2224UserId), case S2225ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2226Proc} -> begin S2227Res = 'Tianjiupai.UserServerSup':'terminate_child'(S2226Proc), begin _ = begin S2736 = 'Tianjiupai.Logger':'info'({"user deleted (user_id: ~p, result: ~p)", 2}, {S2224UserId, S2227Res}), S2736(<<"User.sest">>, 60) end, sesterl_internal_prim:'return'(ok) end end end end.
'get_name'(S2229UserId) -> 'Tianjiupai.UserServer':'get_name'(S2229UserId).
'get_info'(S2231UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S2231UserId).
'set_room'(S2233UserId, S2234RoomIdOpt) -> 'Tianjiupai.UserServer':'set_room'(S2233UserId, S2234RoomIdOpt).
'get_room'(S2236UserId) -> 'Tianjiupai.UserServer':'get_room'(S2236UserId).
'send_chat'(S2238UserId, S2239Text) -> begin S2240UserStateOpt = 'Tianjiupai.User':'get_info'(S2238UserId), case S2240UserStateOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2241UserState} -> case maps:get(belongs_to, S2241UserState) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2242RoomId} -> begin S2243User = #{user_id => S2238UserId, user_name => maps:get(user_name, S2241UserState)}, 'Tianjiupai.Room':'send_chat'(S2242RoomId, S2243User, S2239Text) end end end end.
'ack'(S2245UserId, S2246SnapshotId) -> begin S2247Opt = 'Tianjiupai.User':'get_room'(S2245UserId), case S2247Opt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2248RoomId} -> 'Tianjiupai.Room':'ack'(S2248RoomId, S2245UserId, S2246SnapshotId) end end.
'require_next_inning'(S2250UserId, S2251SnapshotId) -> begin S2252Opt = 'Tianjiupai.User':'get_room'(S2250UserId), case S2252Opt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2253RoomId} -> 'Tianjiupai.Room':'require_next_inning'(S2253RoomId, S2250UserId, S2251SnapshotId) end end.
'set_websocket_connection'(S2255UserId, S2256WsPid) -> 'Tianjiupai.UserServer':'set_websocket_connection'(S2255UserId, S2256WsPid).
