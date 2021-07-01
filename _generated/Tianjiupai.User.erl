-module('Tianjiupai.User').
-export(['generate_user_id'/0, 'create'/1, 'exists'/1, 'get_name'/1, 'get_info'/1, 'set_room'/2, 'get_room'/1, 'send_chat'/2, 'ack'/2, 'require_next_inning'/2, 'set_websocket_connection'/2]).

    generate_user_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S1288UserName) -> begin S1289UserId = 'Tianjiupai.User':'generate_user_id'(), begin S1290Res = 'Tianjiupai.UserServerSup':'start_child'(S1289UserId, S1288UserName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S1291Proc) -> S1289UserId end, S1290Res)) end end.
'exists'(S1293UserId) -> 'Tianjiupai.UserServer':'exists'(S1293UserId).
'get_name'(S1295UserId) -> 'Tianjiupai.UserServer':'get_name'(S1295UserId).
'get_info'(S1297UserId) -> 'Tianjiupai.UserServer':'get_user_state'(S1297UserId).
'set_room'(S1299UserId, S1300RoomId) -> 'Tianjiupai.UserServer':'set_room'(S1299UserId, S1300RoomId).
'get_room'(S1302UserId) -> 'Tianjiupai.UserServer':'get_room'(S1302UserId).
'send_chat'(S1304UserId, S1305Text) -> begin S1306UserStateOpt = 'Tianjiupai.User':'get_info'(S1304UserId), case S1306UserStateOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S1307UserState} -> case maps:get(belongs_to, S1307UserState) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S1308RoomId} -> begin S1309User = #{user_id => S1304UserId, user_name => maps:get(user_name, S1307UserState)}, 'Tianjiupai.Room':'send_chat'(S1308RoomId, S1309User, S1305Text) end end end end.
'ack'(S1311UserId, S1312SnapshotId) -> begin S1313Opt = 'Tianjiupai.User':'get_room'(S1311UserId), case S1313Opt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1314RoomId} -> 'Tianjiupai.Room':'ack'(S1314RoomId, S1311UserId, S1312SnapshotId) end end.
'require_next_inning'(S1316UserId, S1317SnapshotId) -> begin S1318Opt = 'Tianjiupai.User':'get_room'(S1316UserId), case S1318Opt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1319RoomId} -> 'Tianjiupai.Room':'require_next_inning'(S1319RoomId, S1316UserId, S1317SnapshotId) end end.
'set_websocket_connection'(S1321UserId, S1322WsPid) -> 'Tianjiupai.UserServer':'set_websocket_connection'(S1321UserId, S1322WsPid).
