-module('Tianjiupai.UserServer.Callback').
-export(['init'/1, 'monitor_room'/1, 'monitor'/1, 'demonitor'/1, 'handle_call'/3, 'handle_cast'/2, 'check_down_message'/2, 'check_room_down'/2, 'check_websocket_down'/2, 'handle_info'/2, 'terminate'/1]).
'init'(S1112InitArg) -> begin {S1113UserId, S1114UserName} = S1112InitArg, begin S1115Settings = #{user_id => S1113UserId, user_name => S1114UserName}, begin S1116State = #{belongs_to => 'error', settings => S1115Settings, websocket_connection => 'error'}, 'SesterlStdlib.GenServer':'init_ok'(S1116State) end end end.
'monitor_room'(S1118RoomId) -> 'Tianjiupai.Room':'monitor'(S1118RoomId).

      monitor(WsProc) ->
          erlang:monitor(process, WsProc).
    

      demonitor(MonitorRef) ->
          erlang:demonitor(MonitorRef).
    
'handle_call'(S1122Req, S1123From, S1124State) -> case S1122Req of 'get_user_state' -> begin S1127BelongsTo = 'SesterlStdlib.Option':'map'(fun(S1125Pair) -> begin {S1126RoomId, _} = S1125Pair, S1126RoomId end end, maps:get(belongs_to, S1124State)), begin S1128UserState = #{belongs_to => S1127BelongsTo, user_name => maps:get(user_name, maps:get(settings, S1124State))}, 'SesterlStdlib.GenServer':'reply'({'user_state_got', S1128UserState}, S1124State) end end; {'set_room', S1129RoomId} -> begin S1130MonitorRefOpt = 'Tianjiupai.UserServer.Callback':'monitor_room'(S1129RoomId), case S1130MonitorRefOpt of 'error' -> 'SesterlStdlib.GenServer':'reply'({'room_set', false}, S1124State); {'ok', S1131MonitorRefNew} -> begin S1133Dummy = case maps:get(belongs_to, S1124State) of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', {_, S1132MonitorRef}} -> 'Tianjiupai.UserServer.Callback':'demonitor'(S1132MonitorRef) end, begin S1134State = maps:put(belongs_to, {'ok', {S1129RoomId, S1131MonitorRefNew}}, S1124State), 'SesterlStdlib.GenServer':'reply'({'room_set', true}, S1134State) end end end end; {'set_websocket_connection', S1135WsProc} -> begin S1137Dummy = case maps:get(websocket_connection, S1124State) of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S1136MonitorRef} -> 'Tianjiupai.UserServer.Callback':'demonitor'(S1136MonitorRef) end, begin S1138MonitorRefNew = 'Tianjiupai.UserServer.Callback':'monitor'(S1135WsProc), begin S1139State = maps:put(websocket_connection, {'ok', S1138MonitorRefNew}, S1124State), 'SesterlStdlib.GenServer':'reply'('websocket_connection_set', S1139State) end end end end.
'handle_cast'(S1141Msg, S1142State) -> begin _ = sesterl_internal_prim:'print_debug'({<<"unexpected cast message">>, S1141Msg}), 'SesterlStdlib.GenServer':'no_reply'(S1142State) end.

      check_down_message(Info, MonitorRef) ->
        case Info of
            {'DOWN', MonitorRef, process, _Pid, Reason} -> {ok, Reason};
            _                                           -> error
        end.
    
'check_room_down'(S1145Info, S1146State) -> case maps:get(belongs_to, S1146State) of 'error' -> 'error'; {'ok', {_, S1147MonitorRef}} -> 'Tianjiupai.UserServer.Callback':'check_down_message'(S1145Info, S1147MonitorRef) end.
'check_websocket_down'(S1149Info, S1150State) -> case maps:get(websocket_connection, S1150State) of 'error' -> 'error'; {'ok', S1151MonitorRef} -> 'Tianjiupai.UserServer.Callback':'check_down_message'(S1149Info, S1151MonitorRef) end.
'handle_info'(S1153Info, S1154State) -> begin S1155UserId = maps:get(user_id, maps:get(settings, S1154State)), begin S1158State = case 'Tianjiupai.UserServer.Callback':'check_room_down'(S1153Info, S1154State) of {'ok', S1156Reason} -> begin _ = sesterl_internal_prim:'print_debug'(sesterl_internal_prim:'format'({"Room closed (user_id: ~p, reason: ~p)~n", 2}, {S1155UserId, S1156Reason})), maps:put(belongs_to, 'error', S1154State) end; 'error' -> case 'Tianjiupai.UserServer.Callback':'check_websocket_down'(S1153Info, S1154State) of {'ok', S1157Reason} -> begin _ = sesterl_internal_prim:'print_debug'(sesterl_internal_prim:'format'({"WebSocket connection closed (user_id: ~p, reason: ~p)~n", 2}, {S1155UserId, S1157Reason})), maps:put(websocket_connection, 'error', S1154State) end; 'error' -> begin _ = sesterl_internal_prim:'print_debug'(sesterl_internal_prim:'format'({"Unexpected info (user_id: ~p, message: ~p)", 2}, {S1155UserId, S1153Info})), S1154State end end end, 'SesterlStdlib.GenServer':'no_reply'(S1158State) end end.
'terminate'(S1160State) -> begin _ = sesterl_internal_prim:'print_debug'(sesterl_internal_prim:'format'({"terminate (user_id: ~p)", 1}, {maps:get(user_id, maps:get(settings, S1160State))})), sesterl_internal_prim:'return'(ok) end.
