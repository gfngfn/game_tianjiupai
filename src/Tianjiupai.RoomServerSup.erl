-module('Tianjiupai.RoomServerSup').
-export(['start_child_impl'/1, 'start_link'/0, 'start_child'/2, 'as_pid'/1]).
'start_child_impl'(S2168StartArg) -> begin {S2169RoomId, S2170RoomName} = S2168StartArg, 'SesterlStdlib.SupervisorDynamic':'make_child_proc'(fun() -> begin S2171Res = 'Tianjiupai.RoomServer':'start_link'(S2169RoomId, S2170RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'as_pid'/1), S2171Res)) end end) end.
'start_link'() -> 'Tianjiupai.RoomServerSup.Impl':'start_link_name'(ok, {'global', ok}).
'start_child'(S2191RoomId, S2192RoomName) -> begin S2193Opt = 'Tianjiupai.RoomServerSup.Impl':'where_is_global'(ok), case S2193Opt of {'ok', S2194Proc} -> begin S2195Res = 'Tianjiupai.RoomServerSup.Impl':'start_child'(S2194Proc, {S2191RoomId, S2192RoomName}), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'from_pid'/1), S2195Res)) end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"supervisor not found"/utf8>>)}) end end.
'as_pid'(S2197Proc) -> 'Tianjiupai.RoomServerSup.Impl':'as_pid'(S2197Proc).
