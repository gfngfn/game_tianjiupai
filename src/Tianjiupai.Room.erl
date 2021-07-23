-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/1, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/2, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'notify_connected'/2, 'notify_disconnected'/2, 'monitor'/1]).
    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S1966RoomName) -> begin S1967RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S1968Res = 'Tianjiupai.RoomServerSup':'start_child'(S1967RoomId, S1966RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S1969Proc) -> S1967RoomId end, S1968Res)) end end.
'get_all_rooms'() -> begin S1971Procs = 'Tianjiupai.RoomServerSup':'which_children'(), begin S1976Acc = 'SesterlStdlib.List':'foldl_effect'(fun(S1972Acc, S1973Proc) -> begin S1974Opt = 'Tianjiupai.RoomServer':'get_whole_state_by_proc'(S1973Proc), case S1974Opt of {'ok', S1975WholeState} -> sesterl_internal_prim:'return'([S1975WholeState | S1972Acc]); 'error' -> sesterl_internal_prim:'return'(S1972Acc) end end end, [], S1971Procs), sesterl_internal_prim:'return'(S1976Acc) end end.
'get_whole_state'(S1978RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S1978RoomId).
'get_personal_state'(S1980RoomId, S1981UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S1980RoomId, S1981UserId).
'attend'(S1983RoomId, S1984User) -> 'Tianjiupai.RoomServer':'attend'(S1983RoomId, S1984User).
'exit'(S1986RoomId, S1987UserId) -> 'Tianjiupai.RoomServer':'exit'(S1986RoomId, S1987UserId).
'send_chat'(S1989RoomId, S1990From, S1991Text) -> 'Tianjiupai.RoomServer':'send_chat'(S1989RoomId, S1990From, S1991Text).
'submit'(S1993RoomId, S1994UserId, S1995Cards) -> 'Tianjiupai.RoomServer':'submit'(S1993RoomId, S1994UserId, S1995Cards).
'ack'(S1997RoomId, S1998UserId, S1999SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S1997RoomId, S1998UserId, S1999SnapshotId).
'require_next_inning'(S2001RoomId, S2002UserId, S2003SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S2001RoomId, S2002UserId, S2003SnapshotId).
'notify_connected'(S2005RoomId, S2006UserId) -> 'Tianjiupai.RoomServer':'notify_connected'(S2005RoomId, S2006UserId).
'notify_disconnected'(S2008RoomId, S2009UserId) -> 'Tianjiupai.RoomServer':'notify_disconnected'(S2008RoomId, S2009UserId).
'monitor'(S2011RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S2011RoomId).
