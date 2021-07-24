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
  
'monitor'(S1837RoomId) -> 'Tianjiupai.RoomServer':'monitor_impl'('Tianjiupai.RoomServer.Impl', S1837RoomId).
'start_link'(S1839RoomId, S1840RoomName) -> 'Tianjiupai.RoomServer.Impl':'start_link_name'({S1839RoomId, S1840RoomName}, {'global', S1839RoomId}).
'as_pid'(S1842Proc) -> 'Tianjiupai.RoomServer.Impl':'as_pid'(S1842Proc).
'from_pid'(S1844Proc) -> 'Tianjiupai.RoomServer.Impl':'from_pid'(S1844Proc).
'call'(S1846RoomId, S1847Req, S1848F) -> begin S1849ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1846RoomId), case S1849ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S1850Proc} -> begin S1851Resp = 'Tianjiupai.RoomServer.Impl':'call'(S1850Proc, S1847Req), S1848F(S1851Resp) end end end.
'get_whole_state'(S1853RoomId) -> 'Tianjiupai.RoomServer':'call'(S1853RoomId, 'get_whole_state', fun(S1854Resp) -> begin {'whole_state', S1855WholeState} = S1854Resp, sesterl_internal_prim:'return'({'ok', S1855WholeState}) end end).
'get_whole_state_by_proc'(S1857Proc) -> begin S1858Resp = 'Tianjiupai.RoomServer.Impl':'call'(S1857Proc, 'get_whole_state'), begin {'whole_state', S1859WholeState} = S1858Resp, sesterl_internal_prim:'return'({'ok', S1859WholeState}) end end.
'get_personal_state'(S1861RoomId, S1862UserId) -> 'Tianjiupai.RoomServer':'call'(S1861RoomId, {'get_personal_state', S1862UserId}, fun(S1863Resp) -> begin {'personal_state', S1864PersonalStateOpt} = S1863Resp, sesterl_internal_prim:'return'(S1864PersonalStateOpt) end end).
'send_chat'(S1866RoomId, S1867User, S1868Text) -> 'Tianjiupai.RoomServer':'call'(S1866RoomId, {'send_chat', S1867User, S1868Text}, fun(S1869Resp) -> begin 'chat_sent' = S1869Resp, sesterl_internal_prim:'return'({'ok', ok}) end end).
'attend'(S1871RoomId, S1872User) -> 'Tianjiupai.RoomServer':'call'(S1871RoomId, {'attend', S1872User}, fun(S1873Resp) -> begin {'attended', S1874GameStateOpt} = S1873Resp, sesterl_internal_prim:'return'(S1874GameStateOpt) end end).
'exit'(S1876RoomId, S1877UserId) -> 'Tianjiupai.RoomServer':'call'(S1876RoomId, {'exit', S1877UserId}, fun(S1878Resp) -> case S1878Resp of {'exited', false} -> sesterl_internal_prim:'return'('error'); {'exited', true} -> sesterl_internal_prim:'return'({'ok', ok}) end end).
'submit'(S1880RoomId, S1881UserId, S1882Cards) -> 'Tianjiupai.RoomServer':'call'(S1880RoomId, {'submit', S1881UserId, S1882Cards}, fun(S1883Resp) -> begin {'submission_done', S1884V} = S1883Resp, sesterl_internal_prim:'return'(S1884V) end end).
'ack'(S1886RoomId, S1887UserId, S1888SnapshotId) -> begin S1889ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1886RoomId), case S1889ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1890Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1890Proc, {'ack', S1887UserId, S1888SnapshotId}) end end.
'require_next_inning'(S1892RoomId, S1893UserId, S1894SnapshotId) -> begin S1895ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1892RoomId), case S1895ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1896Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1896Proc, {'require_next_inning', S1893UserId, S1894SnapshotId}) end end.
'set_connection'(S1898RoomId, S1899UserId, S1900IsConnected) -> begin S1901ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1898RoomId), case S1901ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1902Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1902Proc, {'set_connection_status', S1899UserId, S1900IsConnected}) end end.
'notify_connected'(S1904RoomId, S1905UserId) -> 'Tianjiupai.RoomServer':'set_connection'(S1904RoomId, S1905UserId, true).
'notify_disconnected'(S1907RoomId, S1908UserId) -> 'Tianjiupai.RoomServer':'set_connection'(S1907RoomId, S1908UserId, false).
