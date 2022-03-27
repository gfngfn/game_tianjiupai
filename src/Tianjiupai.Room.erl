-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/2, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/3, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3, 'monitor'/1]).
    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2311UserId, S2312RoomName) -> begin S2313RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S2314Res = 'Tianjiupai.RoomResourceServer':'add'(S2311UserId, S2313RoomId, S2312RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2313RoomId end, S2314Res)) end end.
'get_all_rooms'() -> 'Tianjiupai.PlazaServer':'get_all_rooms'().
'get_whole_state'(S2317RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S2317RoomId).
'get_personal_state'(S2319RoomId, S2320UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S2319RoomId, S2320UserId).
'attend'(S2322RoomId, S2323User, S2324WsProc) -> 'Tianjiupai.RoomServer':'attend'(S2322RoomId, S2323User, S2324WsProc).
'exit'(S2326RoomId, S2327UserId) -> 'Tianjiupai.RoomServer':'exit'(S2326RoomId, S2327UserId).
'send_chat'(S2329RoomId, S2330From, S2331Text) -> 'Tianjiupai.RoomServer':'send_chat'(S2329RoomId, S2330From, S2331Text).
'submit'(S2333RoomId, S2334UserId, S2335Cards) -> 'Tianjiupai.RoomServer':'submit'(S2333RoomId, S2334UserId, S2335Cards).
'ack'(S2337RoomId, S2338UserId, S2339SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S2337RoomId, S2338UserId, S2339SnapshotId).
'require_next_inning'(S2341RoomId, S2342UserId, S2343SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S2341RoomId, S2342UserId, S2343SnapshotId).
'set_connection'(S2345RoomId, S2346UserId, S2347WsProc) -> 'Tianjiupai.RoomServer':'set_connection'(S2345RoomId, S2346UserId, S2347WsProc).
'monitor'(S2349RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S2349RoomId).
