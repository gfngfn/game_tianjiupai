-module('Tianjiupai.RoomServer').
-export(['monitor'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_whole_state'/1, 'get_personal_state'/2, 'send_chat'/3, 'attend'/3, 'exit'/2, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3]).
'monitor'(S2099RoomId) -> begin S2100ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2099RoomId), case S2100ProcOpt of {'ok', S2101Proc} -> begin S2102Mref = 'SesterlStdlib.MonitorRef':'monitor'('Tianjiupai.RoomServer.Impl':'as_pid'(S2101Proc)), sesterl_internal_prim:'return'({'ok', S2102Mref}) end; 'error' -> sesterl_internal_prim:'return'('error') end end.
'start_link'(S2104RoomId, S2105RoomName) -> 'Tianjiupai.RoomServer.Impl':'start_link_name'({S2104RoomId, S2105RoomName}, {'global', S2104RoomId}).
'as_pid'(S2107Proc) -> 'Tianjiupai.RoomServer.Impl':'as_pid'(S2107Proc).
'from_pid'(S2109Proc) -> 'Tianjiupai.RoomServer.Impl':'from_pid'(S2109Proc).
'call'(S2111RoomId, S2112Req, S2113F) -> begin S2114ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2111RoomId), case S2114ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2115Proc} -> begin S2116RespResult = 'Tianjiupai.RoomServer.Impl':'call'(S2115Proc, S2112Req), case S2116RespResult of {'ok', S2117Resp} -> S2113F(S2117Resp); {'error', S2118Err} -> begin _ = begin S3303 = 'Tianjiupai.Logger':'warning'({"call failed (room_id: ~s, req: ~p, proc: ~p, error: ~p)", 4}, {S2111RoomId, S2112Req, S2115Proc, S2118Err}), S3303(<<"RoomServer.sest">>, 1564) end, sesterl_internal_prim:'return'('error') end end end end end.
'get_whole_state'(S2120RoomId) -> 'Tianjiupai.RoomServer':'call'(S2120RoomId, 'get_whole_state', fun({'whole_state', S2121WholeState}) -> sesterl_internal_prim:'return'({'ok', S2121WholeState}) end).
'get_personal_state'(S2123RoomId, S2124UserId) -> 'Tianjiupai.RoomServer':'call'(S2123RoomId, {'get_personal_state', S2124UserId}, fun({'personal_state', S2125PersonalStateOpt}) -> sesterl_internal_prim:'return'(S2125PersonalStateOpt) end).
'send_chat'(S2127RoomId, S2128User, S2129Text) -> 'Tianjiupai.RoomServer':'call'(S2127RoomId, {'send_chat', S2128User, S2129Text}, fun('chat_sent') -> sesterl_internal_prim:'return'({'ok', ok}) end).
'attend'(S2131RoomId, S2132User, S2133WsProc) -> 'Tianjiupai.RoomServer':'call'(S2131RoomId, {'attend', S2132User, S2133WsProc}, fun({'attended', S2134GameStateOpt}) -> sesterl_internal_prim:'return'(S2134GameStateOpt) end).
'exit'(S2136RoomId, S2137UserId) -> 'Tianjiupai.RoomServer':'call'(S2136RoomId, {'exit', S2137UserId}, fun({'exited', S2138Success}) -> case S2138Success of true -> sesterl_internal_prim:'return'({'ok', ok}); false -> sesterl_internal_prim:'return'('error') end end).
'submit'(S2140RoomId, S2141UserId, S2142Cards) -> 'Tianjiupai.RoomServer':'call'(S2140RoomId, {'submit', S2141UserId, S2142Cards}, fun({'submission_done', S2143V}) -> sesterl_internal_prim:'return'(S2143V) end).
'ack'(S2145RoomId, S2146UserId, S2147SnapshotId) -> begin S2148ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2145RoomId), case S2148ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2149Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S2149Proc, {'ack', S2146UserId, S2147SnapshotId}) end end.
'require_next_inning'(S2151RoomId, S2152UserId, S2153SnapshotId) -> begin S2154ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2151RoomId), case S2154ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2155Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S2155Proc, {'require_next_inning', S2152UserId, S2153SnapshotId}) end end.
'set_connection'(S2157RoomId, S2158UserId, S2159WsProc) -> begin S2160ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2157RoomId), case S2160ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2161Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S2161Proc, {'set_connection', S2158UserId, S2159WsProc}) end end.
