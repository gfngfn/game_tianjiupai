-module('Tianjiupai.RoomServerSup').
-export(['start_child_impl'/1, 'start_link'/0, 'start_child'/2, 'as_pid'/1]).
'start_child_impl'(S2151StartArg) -> begin {S2152RoomId, S2153RoomName} = S2151StartArg, 'SesterlStdlib.SupervisorDynamic':'make_child_proc'(fun() -> begin S2154Res = 'Tianjiupai.RoomServer':'start_link'(S2152RoomId, S2153RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'as_pid'/1), S2154Res)) end end) end.
'start_link'() -> 'Tianjiupai.RoomServerSup.Impl':'start_link_name'(ok, {'global', ok}).
'start_child'(S2174RoomId, S2175RoomName) -> begin S2176Opt = 'Tianjiupai.RoomServerSup.Impl':'where_is_global'(ok), case S2176Opt of {'ok', S2177Proc} -> begin S2178Res = 'Tianjiupai.RoomServerSup.Impl':'start_child'(S2177Proc, {S2174RoomId, S2175RoomName}), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'from_pid'/1), S2178Res)) end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"supervisor not found"/utf8>>)}) end end.
'as_pid'(S2180Proc) -> 'Tianjiupai.RoomServerSup.Impl':'as_pid'(S2180Proc).
