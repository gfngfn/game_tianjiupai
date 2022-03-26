-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/2, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/3, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3, 'monitor'/1]).
    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2303UserId, S2304RoomName) -> begin S2305RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S2306Res = 'Tianjiupai.RoomResourceServer':'add'(S2303UserId, S2305RoomId, S2304RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2305RoomId end, S2306Res)) end end.
'get_all_rooms'() -> 'Tianjiupai.PlazaServer':'get_all_rooms'().
'get_whole_state'(S2309RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S2309RoomId).
'get_personal_state'(S2311RoomId, S2312UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S2311RoomId, S2312UserId).
'attend'(S2314RoomId, S2315User, S2316WsProc) -> 'Tianjiupai.RoomServer':'attend'(S2314RoomId, S2315User, S2316WsProc).
'exit'(S2318RoomId, S2319UserId) -> 'Tianjiupai.RoomServer':'exit'(S2318RoomId, S2319UserId).
'send_chat'(S2321RoomId, S2322From, S2323Text) -> 'Tianjiupai.RoomServer':'send_chat'(S2321RoomId, S2322From, S2323Text).
'submit'(S2325RoomId, S2326UserId, S2327Cards) -> 'Tianjiupai.RoomServer':'submit'(S2325RoomId, S2326UserId, S2327Cards).
'ack'(S2329RoomId, S2330UserId, S2331SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S2329RoomId, S2330UserId, S2331SnapshotId).
'require_next_inning'(S2333RoomId, S2334UserId, S2335SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S2333RoomId, S2334UserId, S2335SnapshotId).
'set_connection'(S2337RoomId, S2338UserId, S2339WsProc) -> 'Tianjiupai.RoomServer':'set_connection'(S2337RoomId, S2338UserId, S2339WsProc).
'monitor'(S2341RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S2341RoomId).
