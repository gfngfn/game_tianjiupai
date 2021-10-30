-module('Tianjiupai.RoomServer').
-export(['monitor'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_whole_state'/1, 'get_personal_state'/2, 'send_chat'/3, 'attend'/3, 'exit'/2, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3]).
'monitor'(S2082RoomId) -> begin S2083ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2082RoomId), case S2083ProcOpt of {'ok', S2084Proc} -> begin S2085Mref = 'SesterlStdlib.MonitorRef':'monitor'('Tianjiupai.RoomServer.Impl':'as_pid'(S2084Proc)), sesterl_internal_prim:'return'({'ok', S2085Mref}) end; 'error' -> sesterl_internal_prim:'return'('error') end end.
'start_link'(S2087RoomId, S2088RoomName) -> 'Tianjiupai.RoomServer.Impl':'start_link_name'({S2087RoomId, S2088RoomName}, {'global', S2087RoomId}).
'as_pid'(S2090Proc) -> 'Tianjiupai.RoomServer.Impl':'as_pid'(S2090Proc).
'from_pid'(S2092Proc) -> 'Tianjiupai.RoomServer.Impl':'from_pid'(S2092Proc).
'call'(S2094RoomId, S2095Req, S2096F) -> begin S2097ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2094RoomId), case S2097ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2098Proc} -> begin S2099RespResult = 'Tianjiupai.RoomServer.Impl':'call'(S2098Proc, S2095Req), case S2099RespResult of {'ok', S2100Resp} -> S2096F(S2100Resp); {'error', S2101Err} -> begin _ = begin S2969 = 'Tianjiupai.Logger':'warning'({"call failed (room_id: ~s, req: ~p, proc: ~p, error: ~p)", 4}, {S2094RoomId, S2095Req, S2098Proc, S2101Err}), S2969(<<"RoomServer.sest">>, 1564) end, sesterl_internal_prim:'return'('error') end end end end end.
'get_whole_state'(S2103RoomId) -> 'Tianjiupai.RoomServer':'call'(S2103RoomId, 'get_whole_state', fun({'whole_state', S2104WholeState}) -> sesterl_internal_prim:'return'({'ok', S2104WholeState}) end).
'get_personal_state'(S2106RoomId, S2107UserId) -> 'Tianjiupai.RoomServer':'call'(S2106RoomId, {'get_personal_state', S2107UserId}, fun({'personal_state', S2108PersonalStateOpt}) -> sesterl_internal_prim:'return'(S2108PersonalStateOpt) end).
'send_chat'(S2110RoomId, S2111User, S2112Text) -> 'Tianjiupai.RoomServer':'call'(S2110RoomId, {'send_chat', S2111User, S2112Text}, fun('chat_sent') -> sesterl_internal_prim:'return'({'ok', ok}) end).
'attend'(S2114RoomId, S2115User, S2116WsProc) -> 'Tianjiupai.RoomServer':'call'(S2114RoomId, {'attend', S2115User, S2116WsProc}, fun({'attended', S2117GameStateOpt}) -> sesterl_internal_prim:'return'(S2117GameStateOpt) end).
'exit'(S2119RoomId, S2120UserId) -> 'Tianjiupai.RoomServer':'call'(S2119RoomId, {'exit', S2120UserId}, fun({'exited', S2121Success}) -> case S2121Success of true -> sesterl_internal_prim:'return'({'ok', ok}); false -> sesterl_internal_prim:'return'('error') end end).
'submit'(S2123RoomId, S2124UserId, S2125Cards) -> 'Tianjiupai.RoomServer':'call'(S2123RoomId, {'submit', S2124UserId, S2125Cards}, fun({'submission_done', S2126V}) -> sesterl_internal_prim:'return'(S2126V) end).
'ack'(S2128RoomId, S2129UserId, S2130SnapshotId) -> begin S2131ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2128RoomId), case S2131ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2132Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S2132Proc, {'ack', S2129UserId, S2130SnapshotId}) end end.
'require_next_inning'(S2134RoomId, S2135UserId, S2136SnapshotId) -> begin S2137ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2134RoomId), case S2137ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2138Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S2138Proc, {'require_next_inning', S2135UserId, S2136SnapshotId}) end end.
'set_connection'(S2140RoomId, S2141UserId, S2142WsProc) -> begin S2143ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S2140RoomId), case S2143ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2144Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S2144Proc, {'set_connection', S2141UserId, S2142WsProc}) end end.
