-module('Tianjiupai.UserServer').
-export(['get_proc'/1, 'exists'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_user_state'/1, 'get_name'/1, 'get_room'/1, 'set_room'/2, 'set_websocket_connection'/2]).
'get_proc'(S1197UserId) -> 'Tianjiupai.UserServer.Impl':'where_is_global'(S1197UserId).
'exists'(S1199UserId) -> begin S1200Opt = 'Tianjiupai.UserServer':'get_proc'(S1199UserId), case S1200Opt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', _} -> sesterl_internal_prim:'return'(true) end end.
'start_link'(S1202UserId, S1203UserName) -> 'Tianjiupai.UserServer.Impl':'start_link_name'({S1202UserId, S1203UserName}, {'global', S1202UserId}).
'as_pid'(S1205Proc) -> 'Tianjiupai.UserServer.Impl':'as_pid'(S1205Proc).
'from_pid'(S1207Pid) -> 'Tianjiupai.UserServer.Impl':'from_pid'(S1207Pid).
'call'(S1209UserId, S1210Req, S1211F) -> begin S1212ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S1209UserId), case S1212ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S1213Proc} -> begin S1214Resp = 'Tianjiupai.UserServer.Impl':'call'(S1213Proc, S1210Req), S1211F(S1214Resp) end end end.
'get_user_state'(S1216UserId) -> 'Tianjiupai.UserServer':'call'(S1216UserId, 'get_user_state', fun(S1217Resp) -> begin {'user_state_got', S1218UserState} = S1217Resp, sesterl_internal_prim:'return'({'ok', S1218UserState}) end end).
'get_name'(S1220UserId) -> begin S1221Opt = 'Tianjiupai.UserServer':'get_user_state'(S1220UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'map'(fun(S1222U) -> maps:get(user_name, S1222U) end, S1221Opt)) end.
'get_room'(S1224UserId) -> begin S1225Opt = 'Tianjiupai.UserServer':'get_user_state'(S1224UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'bind'(S1225Opt, fun(S1226U) -> maps:get(belongs_to, S1226U) end)) end.
'set_room'(S1228UserId, S1229RoomId) -> 'Tianjiupai.UserServer':'call'(S1228UserId, {'set_room', S1229RoomId}, fun(S1230Resp) -> case S1230Resp of {'room_set', true} -> sesterl_internal_prim:'return'({'ok', ok}); {'room_set', false} -> sesterl_internal_prim:'return'('error') end end).
'set_websocket_connection'(S1232UserId, S1233WsProc) -> 'Tianjiupai.UserServer':'call'(S1232UserId, {'set_websocket_connection', S1233WsProc}, fun(S1234Resp) -> begin 'websocket_connection_set' = S1234Resp, sesterl_internal_prim:'return'({'ok', ok}) end end).
