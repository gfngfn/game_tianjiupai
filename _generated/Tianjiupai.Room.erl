-module('Tianjiupai.Room').
-export(['generate_room_id'/0, 'create'/1, 'get_all_rooms'/0, 'get_whole_state'/1, 'get_personal_state'/2, 'attend'/2, 'exit'/2, 'send_chat'/3, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'monitor'/1]).

    generate_room_id() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'create'(S1061RoomName) -> begin S1062RoomId = 'Tianjiupai.Room':'generate_room_id'(), begin S1063Res = 'Tianjiupai.RoomServerSup':'start_child'(S1062RoomId, S1061RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S1064Proc) -> S1062RoomId end, S1063Res)) end end.
'get_all_rooms'() -> begin S1066Procs = 'Tianjiupai.RoomServerSup':'which_children'(), begin S1071Acc = 'SesterlStdlib.List':'foldl_effect'(fun(S1067Acc, S1068Proc) -> begin S1069Opt = 'Tianjiupai.RoomServer':'get_whole_state_by_proc'(S1068Proc), case S1069Opt of {'ok', S1070WholeState} -> sesterl_internal_prim:'return'([S1070WholeState | S1067Acc]); 'error' -> sesterl_internal_prim:'return'(S1067Acc) end end end, [], S1066Procs), sesterl_internal_prim:'return'(S1071Acc) end end.
'get_whole_state'(S1073RoomId) -> 'Tianjiupai.RoomServer':'get_whole_state'(S1073RoomId).
'get_personal_state'(S1075RoomId, S1076UserId) -> 'Tianjiupai.RoomServer':'get_personal_state'(S1075RoomId, S1076UserId).
'attend'(S1078RoomId, S1079User) -> 'Tianjiupai.RoomServer':'attend'(S1078RoomId, S1079User).
'exit'(S1081RoomId, S1082UserId) -> 'Tianjiupai.RoomServer':'exit'(S1081RoomId, S1082UserId).
'send_chat'(S1084RoomId, S1085From, S1086Text) -> 'Tianjiupai.RoomServer':'send_chat'(S1084RoomId, S1085From, S1086Text).
'submit'(S1088RoomId, S1089UserId, S1090Cards) -> 'Tianjiupai.RoomServer':'submit'(S1088RoomId, S1089UserId, S1090Cards).
'ack'(S1092RoomId, S1093UserId, S1094SnapshotId) -> 'Tianjiupai.RoomServer':'ack'(S1092RoomId, S1093UserId, S1094SnapshotId).
'require_next_inning'(S1096RoomId, S1097UserId, S1098SnapshotId) -> 'Tianjiupai.RoomServer':'require_next_inning'(S1096RoomId, S1097UserId, S1098SnapshotId).
'monitor'(S1100RoomId) -> 'Tianjiupai.RoomServer':'monitor'(S1100RoomId).
