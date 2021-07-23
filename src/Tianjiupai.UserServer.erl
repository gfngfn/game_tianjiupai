-module('Tianjiupai.UserServer').
-export(['get_proc'/1, 'exists'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_user_state'/1, 'get_name'/1, 'get_room'/1, 'set_room'/2, 'set_websocket_connection'/2]).
'get_proc'(S2117UserId) -> 'Tianjiupai.UserServer.Impl':'where_is_global'(S2117UserId).
'exists'(S2119UserId) -> begin S2120Opt = 'Tianjiupai.UserServer':'get_proc'(S2119UserId), case S2120Opt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', _} -> sesterl_internal_prim:'return'(true) end end.
'start_link'(S2122UserId, S2123UserName) -> 'Tianjiupai.UserServer.Impl':'start_link_name'({S2122UserId, S2123UserName}, {'global', S2122UserId}).
'as_pid'(S2125Proc) -> 'Tianjiupai.UserServer.Impl':'as_pid'(S2125Proc).
'from_pid'(S2127Pid) -> 'Tianjiupai.UserServer.Impl':'from_pid'(S2127Pid).
'call'(S2129UserId, S2130Req, S2131F) -> begin S2132ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2129UserId), case S2132ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2133Proc} -> begin S2134Resp = 'Tianjiupai.UserServer.Impl':'call'(S2133Proc, S2130Req), S2131F(S2134Resp) end end end.
'get_user_state'(S2136UserId) -> 'Tianjiupai.UserServer':'call'(S2136UserId, 'get_user_state', fun(S2137Resp) -> begin {'user_state_got', S2138UserState} = S2137Resp, sesterl_internal_prim:'return'({'ok', S2138UserState}) end end).
'get_name'(S2140UserId) -> begin S2141Opt = 'Tianjiupai.UserServer':'get_user_state'(S2140UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'map'(fun(S2142U) -> maps:get(user_name, S2142U) end, S2141Opt)) end.
'get_room'(S2144UserId) -> begin S2145Opt = 'Tianjiupai.UserServer':'get_user_state'(S2144UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'bind'(S2145Opt, fun(S2146U) -> maps:get(belongs_to, S2146U) end)) end.
'set_room'(S2148UserId, S2149RoomIdOpt) -> 'Tianjiupai.UserServer':'call'(S2148UserId, {'set_room', S2149RoomIdOpt}, fun(S2150Resp) -> case S2150Resp of {'room_set', true} -> sesterl_internal_prim:'return'({'ok', ok}); {'room_set', false} -> sesterl_internal_prim:'return'('error') end end).
'set_websocket_connection'(S2152UserId, S2153WsProc) -> 'Tianjiupai.UserServer':'call'(S2152UserId, {'set_websocket_connection', S2153WsProc}, fun(S2154Resp) -> begin 'websocket_connection_set' = S2154Resp, sesterl_internal_prim:'return'({'ok', ok}) end end).
