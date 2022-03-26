-module('Tianjiupai.RoomServerSup').
-export(['start_child_impl'/1, 'start_link'/0, 'start_child'/2, 'as_pid'/1]).
'start_child_impl'(S2160StartArg) -> begin {S2161RoomId, S2162RoomName} = S2160StartArg, 'SesterlStdlib.SupervisorDynamic':'make_child_proc'(fun() -> begin S2163Res = 'Tianjiupai.RoomServer':'start_link'(S2161RoomId, S2162RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'as_pid'/1), S2163Res)) end end) end.
'start_link'() -> 'Tianjiupai.RoomServerSup.Impl':'start_link_name'(ok, {'global', ok}).
'start_child'(S2183RoomId, S2184RoomName) -> begin S2185Opt = 'Tianjiupai.RoomServerSup.Impl':'where_is_global'(ok), case S2185Opt of {'ok', S2186Proc} -> begin S2187Res = 'Tianjiupai.RoomServerSup.Impl':'start_child'(S2186Proc, {S2183RoomId, S2184RoomName}), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'from_pid'/1), S2187Res)) end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"supervisor not found"/utf8>>)}) end end.
'as_pid'(S2189Proc) -> 'Tianjiupai.RoomServerSup.Impl':'as_pid'(S2189Proc).
