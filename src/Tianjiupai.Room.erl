-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/2, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/3, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3, 'monitor'/1]).
    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2290UserId, S2291RoomName) -> begin S2292RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S2293Res = 'Tianjiupai.RoomResourceServer':'add'(S2290UserId, S2292RoomId, S2291RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2292RoomId end, S2293Res)) end end.
'get_all_rooms'() -> 'Tianjiupai.PlazaServer':'get_all_rooms'().
'get_whole_state'(S2296RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S2296RoomId).
'get_personal_state'(S2298RoomId, S2299UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S2298RoomId, S2299UserId).
'attend'(S2301RoomId, S2302User, S2303WsProc) -> 'Tianjiupai.RoomServer':'attend'(S2301RoomId, S2302User, S2303WsProc).
'exit'(S2305RoomId, S2306UserId) -> 'Tianjiupai.RoomServer':'exit'(S2305RoomId, S2306UserId).
'send_chat'(S2308RoomId, S2309From, S2310Text) -> 'Tianjiupai.RoomServer':'send_chat'(S2308RoomId, S2309From, S2310Text).
'submit'(S2312RoomId, S2313UserId, S2314Cards) -> 'Tianjiupai.RoomServer':'submit'(S2312RoomId, S2313UserId, S2314Cards).
'ack'(S2316RoomId, S2317UserId, S2318SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S2316RoomId, S2317UserId, S2318SnapshotId).
'require_next_inning'(S2320RoomId, S2321UserId, S2322SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S2320RoomId, S2321UserId, S2322SnapshotId).
'set_connection'(S2324RoomId, S2325UserId, S2326WsProc) -> 'Tianjiupai.RoomServer':'set_connection'(S2324RoomId, S2325UserId, S2326WsProc).
'monitor'(S2328RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S2328RoomId).
