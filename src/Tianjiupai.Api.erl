-module('Tianjiupai.Api').
-export(['create_user'/1, 'is_existent_user'/1, 'delete_user'/1, 'subscribe_plaza'/2, 'get_all_rooms'/0, 'get_personal_state'/3, 'submit'/3, 'create_room'/2, 'enter_room'/3, 'exit_room'/3, 'update_room'/3, 'send_chat'/2, 'ack'/2, 'require_next_inning'/2, 'perform_command'/2, 'set_websocket_connection'/2, 'encode_notification'/1, 'make_flag_user'/1]).
'create_user'(S2474ReqBody) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_create_user_request'(), S2474ReqBody) of {'ok', S2475CreateUserRequest} -> begin S2476UserName = maps:get(user_name, S2475CreateUserRequest), begin S2477Res = 'Tianjiupai.User':'create'(S2476UserName), case S2477Res of {'ok', S2478UserId} -> begin _ = begin S3057 = 'Tianjiupai.Logger':'info'({"user created (user_name: ~s, user_id: ~s)", 2}, {S2476UserName, S2478UserId}), S3057(<<"Api.sest">>, 19) end, begin S2479Enc = 'Tianjiupai.Models':'encode_create_user_response'(), begin S2480RespBody = 'SesterlJson.Encode':'run'(S2479Enc(#{user_id => S2478UserId})), sesterl_internal_prim:'return'({'ok', {S2478UserId, S2480RespBody}}) end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end.
'is_existent_user'(S2482UserId) -> 'Tianjiupai.User':'exists'(S2482UserId).
'delete_user'(S2484UserId) -> begin S2485Deleted = 'Tianjiupai.User':'delete'(S2484UserId), case S2485Deleted of true -> begin _ = begin S3060 = 'Tianjiupai.Logger':'info'({"user deleted (user_id: ~s)", 1}, {S2484UserId}), S3060(<<"Api.sest">>, 40) end, sesterl_internal_prim:'return'(ok) end; false -> sesterl_internal_prim:'return'(ok) end end.
'subscribe_plaza'(S2487UserId, S2488WsProc) -> 'Tianjiupai.PlazaServer':'subscribe'(S2487UserId, S2488WsProc).
'get_all_rooms'() -> begin S2490RoomSummaries = 'Tianjiupai.Room':'get_all_rooms'(), begin S2491Enc = 'Tianjiupai.Models':'encode_get_all_rooms_response'(), sesterl_internal_prim:'return'('SesterlJson.Encode':'run'(S2491Enc(#{rooms => S2490RoomSummaries}))) end end.
'get_personal_state'(S2493RoomId, S2494UserId, S2495Validator) -> case S2495Validator(S2494UserId) of true -> begin S2496Res = 'Tianjiupai.Room':'get_personal_state'(S2493RoomId, S2494UserId), case S2496Res of {'ok', S2497PersonalState} -> begin S2498Enc = 'Tianjiupai.Models':'encode_personal_state'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2498Enc(S2497PersonalState))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end.
'submit'(S2500RoomId, S2501UserId, S2502Cards) -> begin S2503Res = 'Tianjiupai.Room':'submit'(S2500RoomId, S2501UserId, S2502Cards), begin S2504IsSuccess = case S2503Res of {'ok', _} -> true; 'error' -> false end, begin _ = begin S3065 = 'Tianjiupai.Logger':'info'({"submit (room_id: ~s, user_id: ~s, cards: ~p, success: ~p)", 4}, {S2501UserId, S2500RoomId, S2502Cards, S2504IsSuccess}), S3065(<<"Api.sest">>, 75) end, case S2503Res of {'ok', {S2505Obs, S2506TrickLastOpt}} -> begin S2507Enc = 'Tianjiupai.Models':'encode_submit_cards_response'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2507Enc(#{new_state => S2505Obs, trick_last => S2506TrickLastOpt}))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end end end.
'create_room'(S2509ReqBody, S2510Validator) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_create_room_request'(), S2509ReqBody) of {'ok', S2511CreateRoomRequest} -> begin S2512UserId = maps:get(user_id, S2511CreateRoomRequest), begin S2513RoomName = maps:get(room_name, S2511CreateRoomRequest), case S2510Validator(S2512UserId) of true -> begin S2514Res = 'Tianjiupai.Room':'create'(S2513RoomName), case S2514Res of {'ok', S2515RoomId} -> begin _ = begin S3067 = 'Tianjiupai.Logger':'info'({"room created (room_name: ~s, room_id: ~s, created_by: ~s)", 3}, {S2513RoomName, S2515RoomId, S2512UserId}), S3067(<<"Api.sest">>, 98) end, begin S2516Enc = 'Tianjiupai.Models':'encode_create_room_response'(), begin S2517RespBody = 'SesterlJson.Encode':'run'(S2516Enc(#{room_id => S2515RoomId})), sesterl_internal_prim:'return'({'ok', {S2515RoomId, S2517RespBody}}) end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end.
'enter_room'(S2519UserId, S2520RoomId, S2521WsProc) -> begin S2522Res = 'Tianjiupai.User':'set_room'(S2519UserId, {'ok', S2520RoomId}), case S2522Res of {'ok', ok} -> begin {'ok', S2523UserName} = 'Tianjiupai.User':'get_name'(S2519UserId), begin S2524User = #{user_id => S2519UserId, user_name => S2523UserName}, begin S2525Res = 'Tianjiupai.Room':'attend'(S2520RoomId, S2524User, S2521WsProc), begin ok = 'Tianjiupai.PlazaServer':'unsubscribe'(S2519UserId), begin S2526IsSuccess = case S2525Res of {'ok', _} -> true; 'error' -> false end, begin _ = begin S3069 = 'Tianjiupai.Logger':'info'({"attend (user_id: ~s, room_id: ~s, success: ~p)", 3}, {S2519UserId, S2520RoomId, S2526IsSuccess}), S3069(<<"Api.sest">>, 130) end, case S2525Res of {'ok', S2527PersonalState} -> begin S2528Enc = 'Tianjiupai.Models':'encode_enter_room_response'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2528Enc(S2527PersonalState))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end end end end end end; 'error' -> sesterl_internal_prim:'return'('error') end end.
'exit_room'(S2530UserId, S2531RoomId, S2532WsProc) -> begin S2533Res = 'Tianjiupai.User':'set_room'(S2530UserId, 'error'), case S2533Res of {'ok', ok} -> begin S2534Res = 'Tianjiupai.Room':'exit'(S2531RoomId, S2530UserId), begin ok = 'Tianjiupai.PlazaServer':'subscribe'(S2530UserId, S2532WsProc), begin S2535IsSuccess = case S2534Res of {'ok', _} -> true; 'error' -> false end, begin _ = begin S3071 = 'Tianjiupai.Logger':'info'({"exit (user_id: ~s, room_id: ~s, success: ~p)", 3}, {S2530UserId, S2531RoomId, S2535IsSuccess}), S3071(<<"Api.sest">>, 160) end, case S2534Res of {'ok', ok} -> begin S2536Enc = 'Tianjiupai.Models':'encode_exit_room_response'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2536Enc(ok))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end end end end; 'error' -> sesterl_internal_prim:'return'('error') end end.
'update_room'(S2538RoomId, S2539ReqBody, S2540Validator) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_room_request'(), S2539ReqBody) of {'ok', S2541RoomRequest} -> case S2541RoomRequest of {'room_request_to_enter_room', S2542EnterRoomRequest} -> begin S2543UserId = maps:get(user_id, S2542EnterRoomRequest), case S2540Validator(S2543UserId) of true -> begin S2544Opt = 'Tianjiupai.WebSocketHandler':'where_is'(S2543UserId), case S2544Opt of {'ok', S2545WsProc} -> 'Tianjiupai.Api':'enter_room'(S2543UserId, S2538RoomId, S2545WsProc); 'error' -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end end; {'room_request_to_exit_room', S2546ExitRoomRequest} -> begin S2547UserId = maps:get(user_id, S2546ExitRoomRequest), case S2540Validator(S2547UserId) of true -> begin S2548Opt = 'Tianjiupai.WebSocketHandler':'where_is'(S2547UserId), case S2548Opt of {'ok', S2549WsProc} -> 'Tianjiupai.Api':'exit_room'(S2547UserId, S2538RoomId, S2549WsProc); 'error' -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end end; {'room_request_to_submit_cards', S2550SubmitCardsRequest} -> begin S2551UserId = maps:get(user_id, S2550SubmitCardsRequest), begin S2552Cards = maps:get(cards, S2550SubmitCardsRequest), case S2540Validator(S2551UserId) of true -> 'Tianjiupai.Api':'submit'(S2538RoomId, S2551UserId, S2552Cards); false -> sesterl_internal_prim:'return'('error') end end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end.
'send_chat'(S2554UserId, S2555Text) -> begin S2556UserStateOpt = 'Tianjiupai.User':'get_info'(S2554UserId), case S2556UserStateOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2557UserState} -> case maps:get(belongs_to, S2557UserState) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2558RoomId} -> begin S2559User = #{user_id => S2554UserId, user_name => maps:get(user_name, S2557UserState)}, 'Tianjiupai.Room':'send_chat'(S2558RoomId, S2559User, S2555Text) end end end end.
'ack'(S2561UserId, S2562SnapshotId) -> begin S2563RoomIdOpt = 'Tianjiupai.User':'get_room'(S2561UserId), case S2563RoomIdOpt of 'error' -> begin _ = begin S3075 = 'Tianjiupai.Logger':'warning'({"ack failed (user_id: ~s, snapshot_id: ~s)", 2}, {S2561UserId, S2562SnapshotId}), S3075(<<"Api.sest">>, 235) end, sesterl_internal_prim:'return'(ok) end; {'ok', S2564RoomId} -> begin ok = 'Tianjiupai.Room':'ack'(S2564RoomId, S2561UserId, S2562SnapshotId), begin _ = begin S3076 = 'Tianjiupai.Logger':'debug'({"ack (user_id: ~s, snapshot_id: ~s)", 2}, {S2561UserId, S2562SnapshotId}), S3076(<<"Api.sest">>, 240) end, sesterl_internal_prim:'return'(ok) end end end end.
'require_next_inning'(S2566UserId, S2567SnapshotId) -> begin S2568RoomIdOpt = 'Tianjiupai.User':'get_room'(S2566UserId), case S2568RoomIdOpt of 'error' -> sesterl_internal_prim:'return'(ok); {'ok', S2569RoomId} -> 'Tianjiupai.Room':'require_next_inning'(S2569RoomId, S2566UserId, S2567SnapshotId) end end.
'perform_command'(S2571UserId, S2572Data) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_command'(), S2572Data) of {'ok', S2573Command} -> case S2573Command of {'command_comment', S2574Text} -> begin S2575Res = 'Tianjiupai.Api':'send_chat'(S2571UserId, S2574Text), case S2575Res of {'ok', ok} -> sesterl_internal_prim:'return'(ok); 'error' -> begin _ = begin S3079 = 'Tianjiupai.Logger':'warning'({"failed to send a chat comment (user_id: ~s, text: ~s)", 2}, {S2571UserId, S2574Text}), S3079(<<"Api.sest">>, 263) end, sesterl_internal_prim:'return'(ok) end end end; {'command_ack', S2576SnapshotId} -> 'Tianjiupai.Api':'ack'(S2571UserId, S2576SnapshotId); 'command_heartbeat' -> sesterl_internal_prim:'return'(ok); {'command_next_inning', S2577SnapshotId} -> begin S2578Res = 'Tianjiupai.Api':'require_next_inning'(S2571UserId, S2577SnapshotId), begin _ = begin S3080 = 'Tianjiupai.Logger':'debug'({"next inning (user_id: ~s, snapshot_id: ~s, res: ~p)", 3}, {S2571UserId, S2577SnapshotId, S2578Res}), S3080(<<"Api.sest">>, 280) end, sesterl_internal_prim:'return'(ok) end end end; {'error', _} -> sesterl_internal_prim:'return'(ok) end.
'set_websocket_connection'(S2580UserId, S2581WsProc) -> begin S2582RoomOpt = 'Tianjiupai.User':'get_room'(S2580UserId), case S2582RoomOpt of {'ok', S2583RoomId} -> begin _ = 'Tianjiupai.Room':'set_connection'(S2583RoomId, S2580UserId, S2581WsProc), sesterl_internal_prim:'return'(ok) end; 'error' -> begin _ = 'Tianjiupai.Api':'subscribe_plaza'(S2580UserId, S2581WsProc), sesterl_internal_prim:'return'(ok) end end end.
'encode_notification'(S2585Notification) -> begin S2586Enc = 'Tianjiupai.Models':'encode_notification'(), 'SesterlJson.Encode':'run'(S2586Enc(S2585Notification)) end.
'make_flag_user'(S2588UserIdOpt) -> begin S2589Enc = 'Tianjiupai.Models':'encode_flag_user_option'(), begin S2593Opt = case S2588UserIdOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2590UserId} -> begin S2591Res = 'Tianjiupai.User':'get_info'(S2590UserId), case S2591Res of {'ok', S2592Info} -> sesterl_internal_prim:'return'({'ok', #{belongs_to => maps:get(belongs_to, S2592Info), id => S2590UserId, name => maps:get(user_name, S2592Info)}}); 'error' -> sesterl_internal_prim:'return'('error') end end end, sesterl_internal_prim:'return'('SesterlJson.Encode':'run'(S2589Enc(S2593Opt))) end end.