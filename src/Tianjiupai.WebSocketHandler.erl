-module('Tianjiupai.WebSocketHandler').
-export(['where_is'/1, 'notify'/2, 'notify_by_proc'/2, 'notify_room_close'/1, 'monitor'/1]).
    where_is(UserId) ->
        tianjiupai_websocket:where_is(UserId).
  
    notify(UserId, Notifications) ->
        tianjiupai_websocket:notify(UserId, Notifications).
  
    notify_by_proc(WsHandlerPid, Notification) ->
        tianjiupai_websocket:notify_by_proc(WsHandlerPid, Notification).
  
    notify_room_close(UserId) ->
        tianjiupai_websocket:notify_room_close(UserId).
  
'monitor'(S1078WsProc) -> 'SesterlStdlib.MonitorRef':'monitor'(S1078WsProc).
