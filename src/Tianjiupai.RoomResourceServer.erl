-module('Tianjiupai.RoomResourceServer').
-export(['start_link'/0, 'as_pid'/1, 'add'/3, 'get_number_of_rooms'/0]).
'start_link'() -> 'Tianjiupai.RoomResourceServer.Impl':'start_link_name'(ok, {'global', ok}).
'as_pid'(S2272Proc) -> 'Tianjiupai.RoomResourceServer.Impl':'as_pid'(S2272Proc).
'add'(S2274UserId, S2275RoomId, S2276RoomName) -> begin S2277ProcRes = 'Tianjiupai.RoomResourceServer.Impl':'where_is_global'(ok), case S2277ProcRes of {'ok', S2278Proc} -> begin S2279CallRes = 'Tianjiupai.RoomResourceServer.Impl':'call'(S2278Proc, {'add_room', S2274UserId, S2275RoomId, S2276RoomName}), case S2279CallRes of {'ok', {'room_added', S2280Res}} -> sesterl_internal_prim:'return'(S2280Res); {'error', S2281Err} -> sesterl_internal_prim:'return'({'error', S2281Err}) end end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"no room resource server"/utf8>>)}) end end.
'get_number_of_rooms'() -> begin S2283ProcRes = 'Tianjiupai.RoomResourceServer.Impl':'where_is_global'(ok), case S2283ProcRes of {'ok', S2284Proc} -> begin S2285CallRes = 'Tianjiupai.RoomResourceServer.Impl':'call'(S2284Proc, 'get_num_rooms'), case S2285CallRes of {'ok', {'num_rooms_got', S2286N}} -> sesterl_internal_prim:'return'({'ok', S2286N}); {'error', S2287Err} -> sesterl_internal_prim:'return'({'error', S2287Err}) end end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"no room resource server"/utf8>>)}) end end.
