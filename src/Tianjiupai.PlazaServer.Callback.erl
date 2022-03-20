-module('Tianjiupai.PlazaServer.Callback').
-export(['init'/1, 'publish'/1, 'handle_delete_room_sync'/2, 'handle_call'/3, 'monitor_room'/1, 'handle_update_room'/2, 'handle_subscribe'/3, 'handle_unsubscribe'/2, 'handle_cast'/2, 'handle_down'/4, 'handle_timeout'/1, 'handle_info'/2, 'terminate'/2]).
'init'(ok) -> begin S1360State = #{rooms => [], subscribers => []}, 'SesterlStdlib.GenServer':'init_ok'(S1360State) end.
'publish'(S1362State) -> begin S1364RoomSummaries = 'SesterlStdlib.List':'map'(fun(S1363Entry) -> maps:get(room_summary, S1363Entry) end, maps:get(rooms, S1362State)), 'SesterlStdlib.List':'foldl_effect'(fun(ok, S1365Subscriber) -> 'Tianjiupai.WebSocketHandler':'notify_by_proc'(maps:get(proc, S1365Subscriber), [{'notify_plaza_update', S1364RoomSummaries} | []]) end, ok, maps:get(subscribers, S1362State)) end.
'handle_delete_room_sync'(S1367RoomId, S1368State) -> begin S1371EntryAcc = 'SesterlStdlib.List':'foldl_effect'(fun(S1369EntryAcc, S1370Entry) -> case 'SesterlStdlib.Binary':'equal'(maps:get(room_id, maps:get(room, maps:get(room_summary, S1370Entry))), S1367RoomId) of true -> begin _ = 'SesterlStdlib.MonitorRef':'demonitor'(maps:get(monitor, S1370Entry)), sesterl_internal_prim:'return'(S1369EntryAcc) end; false -> sesterl_internal_prim:'return'([S1370Entry | S1369EntryAcc]) end end, [], maps:get(rooms, S1368State)), begin S1372State = maps:put(rooms, 'SesterlStdlib.List':'reverse'(S1371EntryAcc), S1368State), begin _ = 'Tianjiupai.PlazaServer.Callback':'publish'(S1372State), 'SesterlStdlib.GenServer':'reply'('room_deleted', S1372State) end end end.
'handle_call'(S1374Req, _, S1375State) -> case S1374Req of 'get_room_list' -> begin S1377RoomSummaries = 'SesterlStdlib.List':'map'(fun(S1376Entry) -> maps:get(room_summary, S1376Entry) end, maps:get(rooms, S1375State)), 'SesterlStdlib.GenServer':'reply'({'room_list_got', S1377RoomSummaries}, S1375State) end; {'delete_room_sync', S1378RoomId} -> 'Tianjiupai.PlazaServer.Callback':'handle_delete_room_sync'(S1378RoomId, S1375State) end.
      monitor_room(RoomId) ->
          'Tianjiupai.Room':monitor(RoomId).
    
'handle_update_room'(S1381RoomSummary0, S1382State) -> begin {S1386Found, S1387Acc} = 'SesterlStdlib.List':'foldl'(fun({S1383Found, S1384Acc}, S1385Entry) -> case 'SesterlStdlib.Binary':'equal'(maps:get(room_id, maps:get(room, S1381RoomSummary0)), maps:get(room_id, maps:get(room, maps:get(room_summary, S1385Entry)))) of true -> {true, [maps:put(room_summary, S1381RoomSummary0, S1385Entry) | S1384Acc]}; false -> {S1383Found, [S1385Entry | S1384Acc]} end end, {false, []}, maps:get(rooms, S1382State)), begin S1391Acc = case S1386Found of true -> sesterl_internal_prim:'return'(S1387Acc); false -> begin S1388MonitorOpt0 = 'Tianjiupai.PlazaServer.Callback':'monitor_room'(maps:get(room_id, maps:get(room, S1381RoomSummary0))), case S1388MonitorOpt0 of 'error' -> sesterl_internal_prim:'return'(S1387Acc); {'ok', S1389Monitor0} -> begin S1390Entry = #{monitor => S1389Monitor0, room_summary => S1381RoomSummary0}, sesterl_internal_prim:'return'([S1390Entry | S1387Acc]) end end end end, begin S1392RoomEntries = 'SesterlStdlib.List':'reverse'(S1391Acc), begin S1393State = maps:put(rooms, S1392RoomEntries, S1382State), begin _ = begin S3075 = 'Tianjiupai.Logger':'debug'({"update room (room_id: ~s, found: ~p)", 2}, {maps:get(room_id, maps:get(room, S1381RoomSummary0)), S1386Found}), S3075(<<"PlazaServer.sest">>, 144) end, begin _ = 'Tianjiupai.PlazaServer.Callback':'publish'(S1393State), 'SesterlStdlib.GenServer':'no_reply'(S1393State) end end end end end end.
'handle_subscribe'(S1395UserId0, S1396WsProc0, S1397State) -> begin S1398WsMref0 = 'Tianjiupai.WebSocketHandler':'monitor'(S1396WsProc0), begin S1399Subscriber0 = #{monitor => S1398WsMref0, proc => S1396WsProc0, user_id => S1395UserId0}, begin {S1403SubscriberFound, S1404SubscriberAcc} = 'SesterlStdlib.List':'foldl_effect'(fun({S1400SubscriberFound, S1401SubscriberAcc}, S1402Subscriber) -> case 'SesterlStdlib.Binary':'equal'(maps:get(user_id, S1402Subscriber), S1395UserId0) of true -> begin _ = begin S3077 = 'Tianjiupai.Logger':'warning'({"already subscribed (user_id: ~s, old: ~p, new: ~p)", 3}, {S1395UserId0, maps:get(proc, S1402Subscriber), S1396WsProc0}), S3077(<<"PlazaServer.sest">>, 162) end, begin _ = 'SesterlStdlib.MonitorRef':'demonitor'(maps:get(monitor, S1402Subscriber)), sesterl_internal_prim:'return'({true, [S1399Subscriber0 | S1401SubscriberAcc]}) end end; false -> sesterl_internal_prim:'return'({S1400SubscriberFound, [S1402Subscriber | S1401SubscriberAcc]}) end end, {false, []}, maps:get(subscribers, S1397State)), begin S1405SubscriberAcc = case S1403SubscriberFound of true -> S1404SubscriberAcc; false -> [S1399Subscriber0 | S1404SubscriberAcc] end, begin _ = begin S3078 = 'Tianjiupai.Logger':'debug'({"subscribe (user_id: ~s, pid: ~p)", 2}, {S1395UserId0, S1396WsProc0}), S3078(<<"PlazaServer.sest">>, 181) end, 'SesterlStdlib.GenServer':'no_reply'(maps:put(subscribers, 'SesterlStdlib.List':'reverse'(S1405SubscriberAcc), S1397State)) end end end end end.
'handle_unsubscribe'(S1407UserId0, S1408State) -> begin _ = begin S3080 = 'Tianjiupai.Logger':'debug'({"unsubscribe (user_id: ~s)", 1}, {S1407UserId0}), S3080(<<"PlazaServer.sest">>, 185) end, begin S1411SubscriberAcc = 'SesterlStdlib.List':'foldl_effect'(fun(S1409SubscriberAcc, S1410Subscriber) -> case 'SesterlStdlib.Binary':'equal'(maps:get(user_id, S1410Subscriber), S1407UserId0) of true -> begin _ = 'SesterlStdlib.MonitorRef':'demonitor'(maps:get(monitor, S1410Subscriber)), sesterl_internal_prim:'return'(S1409SubscriberAcc) end; false -> sesterl_internal_prim:'return'([S1410Subscriber | S1409SubscriberAcc]) end end, [], maps:get(subscribers, S1408State)), 'SesterlStdlib.GenServer':'no_reply'(maps:put(subscribers, 'SesterlStdlib.List':'reverse'(S1411SubscriberAcc), S1408State)) end end.
'handle_cast'(S1413Msg, S1414State) -> case S1413Msg of {'update_room', S1415RoomSummary0} -> 'Tianjiupai.PlazaServer.Callback':'handle_update_room'(S1415RoomSummary0, S1414State); {'subscribe', S1416UserId, S1417WsProc} -> 'Tianjiupai.PlazaServer.Callback':'handle_subscribe'(S1416UserId, S1417WsProc, S1414State); {'unsubscribe', S1418UserId} -> 'Tianjiupai.PlazaServer.Callback':'handle_unsubscribe'(S1418UserId, S1414State) end.
'handle_down'(S1420Mref, S1421Pid, S1422Reason, S1423State) -> begin {S1427RoomFound, S1428EntryAcc} = 'SesterlStdlib.List':'foldl'(fun({S1424RoomFound, S1425EntryAcc}, S1426Entry) -> case 'SesterlStdlib.MonitorRef':'equal'(maps:get(monitor, S1426Entry), S1420Mref) of true -> begin _ = begin S3083 = 'Tianjiupai.Logger':'debug'({"room down (room_id: ~s, pid: ~p, reason: ~p)", 3}, {maps:get(room_id, maps:get(room, maps:get(room_summary, S1426Entry))), S1421Pid, S1422Reason}), S3083(<<"PlazaServer.sest">>, 213) end, {true, S1425EntryAcc} end; false -> {S1424RoomFound, [S1426Entry | S1425EntryAcc]} end end, {false, []}, maps:get(rooms, S1423State)), case S1427RoomFound of true -> begin S1429State = maps:put(rooms, 'SesterlStdlib.List':'reverse'(S1428EntryAcc), S1423State), begin _ = 'Tianjiupai.PlazaServer.Callback':'publish'(S1429State), 'SesterlStdlib.GenServer':'no_reply'(S1429State) end end; false -> begin {S1433SubscriberFound, S1434SubscriberAcc} = 'SesterlStdlib.List':'foldl'(fun({S1430SubscriberFound, S1431SubscriberAcc}, S1432Subscriber) -> case 'SesterlStdlib.MonitorRef':'equal'(maps:get(monitor, S1432Subscriber), S1420Mref) of true -> begin _ = begin S3084 = 'Tianjiupai.Logger':'debug'({"subscriber down (user_id: ~s, pid: ~p, reason: ~p)", 3}, {maps:get(user_id, S1432Subscriber), S1421Pid, S1422Reason}), S3084(<<"PlazaServer.sest">>, 235) end, {true, S1431SubscriberAcc} end; false -> {S1430SubscriberFound, [S1432Subscriber | S1431SubscriberAcc]} end end, {false, []}, maps:get(subscribers, S1423State)), case S1433SubscriberFound of true -> begin S1435State = maps:put(subscribers, 'SesterlStdlib.List':'reverse'(S1434SubscriberAcc), S1423State), 'SesterlStdlib.GenServer':'no_reply'(S1435State) end; false -> begin _ = begin S3085 = 'Tianjiupai.Logger':'warning'({"unexpected monitor ref (mref: ~p, pid: ~p, reason: ~p)", 3}, {S1420Mref, S1421Pid, S1422Reason}), S3085(<<"PlazaServer.sest">>, 252) end, 'SesterlStdlib.GenServer':'no_reply'(S1423State) end end end end end.
'handle_timeout'(S1437State) -> 'SesterlStdlib.GenServer':'no_reply'(S1437State).
'handle_info'(S1439Info, S1440State) -> begin _ = begin S3088 = 'Tianjiupai.Logger':'warning'({"unexpected info (info: ~p, state: ~p)", 2}, {S1439Info, S1440State}), S3088(<<"PlazaServer.sest">>, 263) end, 'SesterlStdlib.GenServer':'no_reply'(S1440State) end.
'terminate'(S1442Reason, S1443State) -> begin _ = begin S3090 = 'Tianjiupai.Logger':'warning'({"terminate (reason: ~p, state: ~p)", 2}, {S1442Reason, S1443State}), S3090(<<"PlazaServer.sest">>, 267) end, sesterl_internal_prim:'return'(ok) end.
