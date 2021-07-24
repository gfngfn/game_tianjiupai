-module('Tianjiupai.UserServer').
-export(['get_proc'/1, 'exists'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_user_state'/1, 'get_name'/1, 'get_room'/1, 'set_room'/2, 'set_websocket_connection'/2]).
'get_proc'(S2118UserId) -> 'Tianjiupai.UserServer.Impl':'where_is_global'(S2118UserId).
'exists'(S2120UserId) -> begin S2121Opt = 'Tianjiupai.UserServer':'get_proc'(S2120UserId), case S2121Opt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', _} -> sesterl_internal_prim:'return'(true) end end.
'start_link'(S2123UserId, S2124UserName) -> 'Tianjiupai.UserServer.Impl':'start_link_name'({S2123UserId, S2124UserName}, {'global', S2123UserId}).
'as_pid'(S2126Proc) -> 'Tianjiupai.UserServer.Impl':'as_pid'(S2126Proc).
'from_pid'(S2128Pid) -> 'Tianjiupai.UserServer.Impl':'from_pid'(S2128Pid).
'call'(S2130UserId, S2131Req, S2132F) -> begin S2133ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2130UserId), case S2133ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2134Proc} -> begin S2135Resp = 'Tianjiupai.UserServer.Impl':'call'(S2134Proc, S2131Req), S2132F(S2135Resp) end end end.
'get_user_state'(S2137UserId) -> 'Tianjiupai.UserServer':'call'(S2137UserId, 'get_user_state', fun(S2138Resp) -> begin {'user_state_got', S2139UserState} = S2138Resp, sesterl_internal_prim:'return'({'ok', S2139UserState}) end end).
'get_name'(S2141UserId) -> begin S2142Opt = 'Tianjiupai.UserServer':'get_user_state'(S2141UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'map'(fun(S2143U) -> maps:get(user_name, S2143U) end, S2142Opt)) end.
'get_room'(S2145UserId) -> begin S2146Opt = 'Tianjiupai.UserServer':'get_user_state'(S2145UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'bind'(S2146Opt, fun(S2147U) -> maps:get(belongs_to, S2147U) end)) end.
'set_room'(S2149UserId, S2150RoomIdOpt) -> 'Tianjiupai.UserServer':'call'(S2149UserId, {'set_room', S2150RoomIdOpt}, fun(S2151Resp) -> case S2151Resp of {'room_set', true} -> sesterl_internal_prim:'return'({'ok', ok}); {'room_set', false} -> sesterl_internal_prim:'return'('error') end end).
'set_websocket_connection'(S2153UserId, S2154WsProc) -> 'Tianjiupai.UserServer':'call'(S2153UserId, {'set_websocket_connection', S2154WsProc}, fun(S2155Resp) -> begin 'websocket_connection_set' = S2155Resp, sesterl_internal_prim:'return'({'ok', ok}) end end).
