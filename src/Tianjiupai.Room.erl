-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/1, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/3, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3, 'monitor'/1]).
    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S2196RoomName) -> begin S2197RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S2198Res = 'Tianjiupai.RoomServerSup':'start_child'(S2197RoomId, S2196RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(_) -> S2197RoomId end, S2198Res)) end end.
'get_all_rooms'() -> 'Tianjiupai.PlazaServer':'get_all_rooms'().
'get_whole_state'(S2201RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S2201RoomId).
'get_personal_state'(S2203RoomId, S2204UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S2203RoomId, S2204UserId).
'attend'(S2206RoomId, S2207User, S2208WsProc) -> 'Tianjiupai.RoomServer':'attend'(S2206RoomId, S2207User, S2208WsProc).
'exit'(S2210RoomId, S2211UserId) -> 'Tianjiupai.RoomServer':'exit'(S2210RoomId, S2211UserId).
'send_chat'(S2213RoomId, S2214From, S2215Text) -> 'Tianjiupai.RoomServer':'send_chat'(S2213RoomId, S2214From, S2215Text).
'submit'(S2217RoomId, S2218UserId, S2219Cards) -> 'Tianjiupai.RoomServer':'submit'(S2217RoomId, S2218UserId, S2219Cards).
'ack'(S2221RoomId, S2222UserId, S2223SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S2221RoomId, S2222UserId, S2223SnapshotId).
'require_next_inning'(S2225RoomId, S2226UserId, S2227SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S2225RoomId, S2226UserId, S2227SnapshotId).
'set_connection'(S2229RoomId, S2230UserId, S2231WsProc) -> 'Tianjiupai.RoomServer':'set_connection'(S2229RoomId, S2230UserId, S2231WsProc).
'monitor'(S2233RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S2233RoomId).
