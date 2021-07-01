-module('Tianjiupai.RoomServerSup').
-export(['start_child_impl'/1, 'start_link'/0, 'start_child'/2, 'which_children'/0, 'as_pid'/1]).
'start_child_impl'(S1024StartArg) -> begin {S1025RoomId, S1026RoomName} = S1024StartArg, 'SesterlStdlib.DynamicSupervisor':'make_child_proc'(fun() -> begin S1027Res = 'Tianjiupai.RoomServer':'start_link'(S1025RoomId, S1026RoomName), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'as_pid'/1), S1027Res)) end end) end.
'start_link'() -> 'Tianjiupai.RoomServerSup.Impl':'start_link_name'(ok, {'global', ok}).
'start_child'(S1047RoomId, S1048RoomName) -> begin S1049Opt = 'Tianjiupai.RoomServerSup.Impl':'where_is_global'(ok), case S1049Opt of {'ok', S1050Proc} -> begin S1051Res = 'Tianjiupai.RoomServerSup.Impl':'start_child'(S1050Proc, {S1047RoomId, S1048RoomName}), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServer':'from_pid'/1), S1051Res)) end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"supervisor not found">>)}) end end.
'which_children'() -> begin S1053Opt = 'Tianjiupai.RoomServerSup.Impl':'where_is_global'(ok), case S1053Opt of {'ok', S1054Proc} -> begin S1055Pids = 'Tianjiupai.RoomServerSup.Impl':'which_children'(S1054Proc), sesterl_internal_prim:'return'('SesterlStdlib.List':'map'((fun 'Tianjiupai.RoomServer':'from_pid'/1), S1055Pids)) end; 'error' -> sesterl_internal_prim:'return'([]) end end.
'as_pid'(S1057Proc) -> 'Tianjiupai.RoomServerSup.Impl':'as_pid'(S1057Proc).
