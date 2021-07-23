-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'delete'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1, 'send_chat'/2, 'ack'/2, 'require_next_inning'/2, 'set_websocket_connection'/2]).
    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2216UserName) -> begin S2217UserId = 'Tianjiupai.User':'generate_user_id'(), begin S2218Res = 'Tianjiupai.UserServerSup':'start_child'(S2217UserId, S2216UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S2219Proc) -> S2217UserId end, S2218Res)) end end.
'exists'(S2221UserId) -> 'Tianjiupai.UserServer':'exists'(S2221UserId).
'delete'(S2223UserId) -> begin S2224ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2223UserId), case S2224ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2225Proc} -> begin S2226Res = 'Tianjiupai.UserServerSup':'terminate_child'(S2225Proc), begin _ = begin S2735 = 'Tianjiupai.Logger':'info'({"user deleted (user_id: ~p, result: ~p)", 2}, {S2223UserId, S2226Res}), S2735(<<"User.sest">>, 60) end, sesterl_internal_prim:'return'(ok) end end end end.
'get_name'(S2228UserId) -> 'Tianjiupai.UserServer':'get_name'(S2228UserId).
'get_info'(S2230UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S2230UserId).
'set_room'(S2232UserId, S2233RoomIdOpt) -> 'Tianjiupai.UserServer':'set_room'(S2232UserId, S2233RoomIdOpt).
'get_room'(S2235UserId) -> 'Tianjiupai.UserServer':'get_room'(S2235UserId).
'send_chat'(S2237UserId, S2238Text) -> begin S2239UserStateOpt = 'Tianjiupai.User':'get_info'(S2237UserId), case S2239UserStateOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2240UserState} -> case maps:get(belongs_to, S2240UserState) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2241RoomId} -> begin S2242User = #{user_id => S2237UserId, user_name => maps:get(user_name, S2240UserState)}, 'Tianjiupai.Room':'send_chat'(S2241RoomId, S2242User, S2238Text) end end end end.
'ack'(S2244UserId, S2245SnapshotId) -> begin S2246Opt = 'Tianjiupai.User':'get_room'(S2244UserId), case S2246Opt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2247RoomId} -> 'Tianjiupai.Room':'ack'(S2247RoomId, S2244UserId, S2245SnapshotId) end end.
'require_next_inning'(S2249UserId, S2250SnapshotId) -> begin S2251Opt = 'Tianjiupai.User':'get_room'(S2249UserId), case S2251Opt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2252RoomId} -> 'Tianjiupai.Room':'require_next_inning'(S2252RoomId, S2249UserId, S2250SnapshotId) end end.
'set_websocket_connection'(S2254UserId, S2255WsPid) -> 'Tianjiupai.UserServer':'set_websocket_connection'(S2254UserId, S2255WsPid).
