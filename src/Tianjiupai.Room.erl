-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/1, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/2, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'notify_connected'/2, 'notify_disconnected'/2, 'monitor'/1]).
    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S1967RoomName) -> begin S1968RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S1969Res = 'Tianjiupai.RoomServerSup':'start_child'(S1968RoomId, S1967RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S1970Proc) -> S1968RoomId end, S1969Res)) end end.
'get_all_rooms'() -> begin S1972Procs = 'Tianjiupai.RoomServerSup':'which_children'(), begin S1977Acc = 'SesterlStdlib.List':'foldl_effect'(fun(S1973Acc, S1974Proc) -> begin S1975Opt = 'Tianjiupai.RoomServer':'get_whole_state_by_proc'(S1974Proc), case S1975Opt of {'ok', S1976WholeState} -> sesterl_internal_prim:'return'([S1976WholeState | S1973Acc]); 'error' -> sesterl_internal_prim:'return'(S1973Acc) end end end, [], S1972Procs), sesterl_internal_prim:'return'(S1977Acc) end end.
'get_whole_state'(S1979RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S1979RoomId).
'get_personal_state'(S1981RoomId, S1982UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S1981RoomId, S1982UserId).
'attend'(S1984RoomId, S1985User) -> 'Tianjiupai.RoomServer':'attend'(S1984RoomId, S1985User).
'exit'(S1987RoomId, S1988UserId) -> 'Tianjiupai.RoomServer':'exit'(S1987RoomId, S1988UserId).
'send_chat'(S1990RoomId, S1991From, S1992Text) -> 'Tianjiupai.RoomServer':'send_chat'(S1990RoomId, S1991From, S1992Text).
'submit'(S1994RoomId, S1995UserId, S1996Cards) -> 'Tianjiupai.RoomServer':'submit'(S1994RoomId, S1995UserId, S1996Cards).
'ack'(S1998RoomId, S1999UserId, S2000SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S1998RoomId, S1999UserId, S2000SnapshotId).
'require_next_inning'(S2002RoomId, S2003UserId, S2004SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S2002RoomId, S2003UserId, S2004SnapshotId).
'notify_connected'(S2006RoomId, S2007UserId) -> 'Tianjiupai.RoomServer':'notify_connected'(S2006RoomId, S2007UserId).
'notify_disconnected'(S2009RoomId, S2010UserId) -> 'Tianjiupai.RoomServer':'notify_disconnected'(S2009RoomId, S2010UserId).
'monitor'(S2012RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S2012RoomId).
