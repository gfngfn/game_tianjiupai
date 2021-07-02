-module('Tianjiupai.RoomServer').
-export(['todo'/1, 'monitor_impl'/2, 'monitor'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_whole_state'/1, 'get_whole_state_by_proc'/1, 'get_personal_state'/2, 'send_chat'/3, 'attend'/2, 'exit'/2, 'submit'/3, 'ack'/3, 'require_next_inning'/3]).

    todo(X) ->
      erlang:error({todo, X}).
  

    monitor_impl(Impl, RoomId) ->
        case Impl:where_is_global(RoomId) of
            error     -> error;
            {ok, Pid} -> {ok, erlang:monitor(process, Pid)}
        end.
  
'monitor'(S957RoomId) -> 'Tianjiupai.RoomServer':'monitor_impl'('Tianjiupai.RoomServer.Impl', S957RoomId).
'start_link'(S959RoomId, S960RoomName) -> 'Tianjiupai.RoomServer.Impl':'start_link_name'({S959RoomId, S960RoomName}, {'global', S959RoomId}).
'as_pid'(S962Proc) -> 'Tianjiupai.RoomServer.Impl':'as_pid'(S962Proc).
'from_pid'(S964Proc) -> 'Tianjiupai.RoomServer.Impl':'from_pid'(S964Proc).
'call'(S966RoomId, S967Req, S968F) -> begin S969ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S966RoomId), case S969ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S970Proc} -> begin S971Resp = 'Tianjiupai.RoomServer.Impl':'call'(S970Proc, S967Req), S968F(S971Resp) end end end.
'get_whole_state'(S973RoomId) -> 'Tianjiupai.RoomServer':'call'(S973RoomId, 'get_whole_state', fun(S974Resp) -> begin {'whole_state', S975WholeState} = S974Resp, sesterl_internal_prim:'return'({'ok', S975WholeState}) end end).
'get_whole_state_by_proc'(S977Proc) -> begin S978Resp = 'Tianjiupai.RoomServer.Impl':'call'(S977Proc, 'get_whole_state'), begin {'whole_state', S979WholeState} = S978Resp, sesterl_internal_prim:'return'({'ok', S979WholeState}) end end.
'get_personal_state'(S981RoomId, S982UserId) -> 'Tianjiupai.RoomServer':'call'(S981RoomId, {'get_personal_state', S982UserId}, fun(S983Resp) -> begin {'personal_state', S984PersonalStateOpt} = S983Resp, sesterl_internal_prim:'return'(S984PersonalStateOpt) end end).
'send_chat'(S986RoomId, S987User, S988Text) -> 'Tianjiupai.RoomServer':'call'(S986RoomId, {'send_chat', S987User, S988Text}, fun(S989Resp) -> begin 'chat_sent' = S989Resp, sesterl_internal_prim:'return'({'ok', ok}) end end).
'attend'(S991RoomId, S992User) -> 'Tianjiupai.RoomServer':'call'(S991RoomId, {'attend', S992User}, fun(S993Resp) -> begin {'attended', S994GameStateOpt} = S993Resp, sesterl_internal_prim:'return'(S994GameStateOpt) end end).
'exit'(S996RoomId, S997UserId) -> 'Tianjiupai.RoomServer':'call'(S996RoomId, {'exit', S997UserId}, fun(S998Resp) -> case S998Resp of {'exited', false} -> sesterl_internal_prim:'return'('error'); {'exited', true} -> sesterl_internal_prim:'return'({'ok', ok}) end end).
'submit'(S1000RoomId, S1001UserId, S1002Cards) -> 'Tianjiupai.RoomServer':'call'(S1000RoomId, {'submit', S1001UserId, S1002Cards}, fun(S1003Resp) -> begin {'submission_done', S1004V} = S1003Resp, sesterl_internal_prim:'return'(S1004V) end end).
'ack'(S1006RoomId, S1007UserId, S1008SnapshotId) -> begin S1009ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1006RoomId), case S1009ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1010Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1010Proc, {'ack', S1007UserId, S1008SnapshotId}) end end.
'require_next_inning'(S1012RoomId, S1013UserId, S1014SnapshotId) -> begin S1015ProcOpt = 'Tianjiupai.RoomServer.Impl':'where_is_global'(S1012RoomId), case S1015ProcOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1016Proc} -> 'Tianjiupai.RoomServer.Impl':'cast'(S1016Proc, {'require_next_inning', S1013UserId, S1014SnapshotId}) end end.
