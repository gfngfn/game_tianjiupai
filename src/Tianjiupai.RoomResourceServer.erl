-module('Tianjiupai.RoomResourceServer').
-export(['start_link'/0, 'as_pid'/1, 'add'/3]).
'start_link'() -> 'Tianjiupai.RoomResourceServer.Impl':'start_link_name'(ok, {'global', ok}).
'as_pid'(S2265Proc) -> 'Tianjiupai.RoomResourceServer.Impl':'as_pid'(S2265Proc).
'add'(S2267UserId, S2268RoomId, S2269RoomName) -> begin S2270ProcRes = 'Tianjiupai.RoomResourceServer.Impl':'where_is_global'(ok), case S2270ProcRes of {'ok', S2271Proc} -> begin S2272CallRes = 'Tianjiupai.RoomResourceServer.Impl':'call'(S2271Proc, {'add_room', S2267UserId, S2268RoomId, S2269RoomName}), case S2272CallRes of {'ok', {'room_added', S2273Res}} -> sesterl_internal_prim:'return'(S2273Res); {'error', S2274Err} -> sesterl_internal_prim:'return'({'error', S2274Err}) end end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"no room resource server"/utf8>>)}) end end.
