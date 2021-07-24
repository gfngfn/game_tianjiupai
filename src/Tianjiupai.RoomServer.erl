-module('Tianjiupai.RoomServer').
-export(['disconnection_timeout'/0, 'maximum_num_innings'/0, 'normal_atom'/0, 'todo'/1, 'monitor_impl'/2, 'monitor'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_whole_state'/1, 'get_whole_state_by_proc'/1, 'get_personal_state'/2, 'send_chat'/3, 'attend'/2, 'exit'/2, 'submit'/3, 'ack'/3, 'require_next_inning'/3, 'set_connection'/3, 'notify_connected'/2, 'notify_disconnected'/2]).
'disconnection_timeout'() -> 30000.
'maximum_num_innings'() -> 8.
    normal_atom() ->
        normal.
  
    todo(X) ->
      erlang:error({todo, X}).
  
    monitor_impl(Impl, RoomId) ->
        case Impl:where_is_global(RoomId) of
            error     -> error;
            {ok, Pid} -> {ok, erlang:monitor(process, Pid)}
        end.
  
'monitor'(S1836RoomId) -> 'Tianjiupai.RoomServer':'monitor_impl'('Tianjiupai.RoomServer.Impl', S1836RoomId).
'start_link'(S1838RoomId, S1839RoomName) -> 'Tianjiupai.RoomServer.Impl':'start_link_name'({S1838RoomId, S1839RoomName}, {'global', S1838RoomId}).
'as_pid'(S1841Proc) -> 'Tianjiupai.RoomServer.Impl':'as_pid'(S1841Proc).
'from_pid'(S1843Proc) -> 'Tianjiupai.RoomServer.Impl':'from_pid'(S1843Proc).
'call'(S1845RoomId, S1846Req, S1847F) -> begin S1848ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1845RoomId), case S1848ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S1849Proc} -> begin S1850Resp = 'Tianjiupai.RoomServer.Impl':'call'(S1849Proc, S1846Req), S1847F(S1850Resp) end end end.
'get_whole_state'(S1852RoomId) -> 'Tianjiupai.RoomServer':'call'(S1852RoomId, 'get_whole_state', fun(S1853Resp) -> begin {'whole_state', S1854WholeState} = S1853Resp, sesterl_internal_prim:'return'({'ok', S1854WholeState}) end end).
'get_whole_state_by_proc'(S1856Proc) -> begin S1857Resp = 'Tianjiupai.RoomServer.Impl':'call'(S1856Proc, 'get_whole_state'), begin {'whole_state', S1858WholeState} = S1857Resp, sesterl_internal_prim:'return'({'ok', S1858WholeState}) end end.
'get_personal_state'(S1860RoomId, S1861UserId) -> 'Tianjiupai.RoomServer':'call'(S1860RoomId, {'get_personal_state', S1861UserId}, fun(S1862Resp) -> begin {'personal_state', S1863PersonalStateOpt} = S1862Resp, sesterl_internal_prim:'return'(S1863PersonalStateOpt) end end).
'send_chat'(S1865RoomId, S1866User, S1867Text) -> 'Tianjiupai.RoomServer':'call'(S1865RoomId, {'send_chat', S1866User, S1867Text}, fun(S1868Resp) -> begin 'chat_sent' = S1868Resp, sesterl_internal_prim:'return'({'ok', ok}) end end).
'attend'(S1870RoomId, S1871User) -> 'Tianjiupai.RoomServer':'call'(S1870RoomId, {'attend', S1871User}, fun(S1872Resp) -> begin {'attended', S1873GameStateOpt} = S1872Resp, sesterl_internal_prim:'return'(S1873GameStateOpt) end end).
'exit'(S1875RoomId, S1876UserId) -> 'Tianjiupai.RoomServer':'call'(S1875RoomId, {'exit', S1876UserId}, fun(S1877Resp) -> case S1877Resp of {'exited', false} -> sesterl_internal_prim:'return'('error'); {'exited', true} -> sesterl_internal_prim:'return'({'ok', ok}) end end).
'submit'(S1879RoomId, S1880UserId, S1881Cards) -> 'Tianjiupai.RoomServer':'call'(S1879RoomId, {'submit', S1880UserId, S1881Cards}, fun(S1882Resp) -> begin {'submission_done', S1883V} = S1882Resp, sesterl_internal_prim:'return'(S1883V) end end).
'ack'(S1885RoomId, S1886UserId, S1887SnapshotId) -> begin S1888ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1885RoomId), case S1888ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1889Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1889Proc, {'ack', S1886UserId, S1887SnapshotId}) end end.
'require_next_inning'(S1891RoomId, S1892UserId, S1893SnapshotId) -> begin S1894ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1891RoomId), case S1894ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1895Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1895Proc, {'require_next_inning', S1892UserId, S1893SnapshotId}) end end.
'set_connection'(S1897RoomId, S1898UserId, S1899IsConnected) -> begin S1900ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1897RoomId), case S1900ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1901Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1901Proc, {'set_connection_status', S1898UserId, S1899IsConnected}) end end.
'notify_connected'(S1903RoomId, S1904UserId) -> 'Tianjiupai.RoomServer':'set_connection'(S1903RoomId, S1904UserId, true).
'notify_disconnected'(S1906RoomId, S1907UserId) -> 'Tianjiupai.RoomServer':'set_connection'(S1906RoomId, S1907UserId, false).
