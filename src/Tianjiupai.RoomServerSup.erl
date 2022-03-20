-module('Tianjiupai.RoomServerSup').
-export(['start_child_impl'/1, 'start_link'/0, 'start_child'/2, 'as_pid'/1]).
'start_child_impl'(S2154StartArg) -> begin {S2155RoomId, S2156RoomName} = S2154StartArg, 'SesterlStdlib.SupervisorDynamic':'make_child_proc'(fun() -> begin S2157Res = 'Tianjiupai.RoomServer':'start_link'(S2155RoomId, S2156RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'as_pid'/1), S2157Res)) end end) end.
'start_link'() -> 'Tianjiupai.RoomServerSup.Impl':'start_link_name'(ok, {'global', ok}).
'start_child'(S2177RoomId, S2178RoomName) -> begin S2179Opt = 'Tianjiupai.RoomServerSup.Impl':'where_is_global'(ok), case S2179Opt of {'ok', S2180Proc} -> begin S2181Res = 'Tianjiupai.RoomServerSup.Impl':'start_child'(S2180Proc, {S2177RoomId, S2178RoomName}), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'from_pid'/1), S2181Res)) end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"supervisor not found"/utf8>>)}) end end.
'as_pid'(S2183Proc) -> 'Tianjiupai.RoomServerSup.Impl':'as_pid'(S2183Proc).
