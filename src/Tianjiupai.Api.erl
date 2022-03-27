-module('Tianjiupai.Api').
-export(['create_user'/1, 'is_existent_user'/1, 'delete_user'/1, 'subscribe_plaza'/2, 'get_all_rooms'/0, 'get_personal_state'/3, 'submit'/3, 'create_room'/2, 'enter_room'/3, 'exit_room'/3, 'update_room'/3, 'send_chat'/2, 'ack'/2, 'require_next_inning'/2, 'perform_command'/2, 'set_websocket_connection'/2, 'encode_notification'/1, 'make_flag_user'/1]).
'create_user'(S2766ReqBody) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_create_user_request'(), S2766ReqBody) of {'ok', S2767CreateUserRequest} -> begin S2768UserName = maps:get(user_name, S2767CreateUserRequest), begin S2769Res = 'Tianjiupai.User':'create'(S2768UserName), case S2769Res of {'ok', S2770UserId} -> begin _ = begin S3469 = 'Tianjiupai.Logger':'info'({"user created (user_name: ~s, user_id: ~s)", 2}, {S2768UserName, S2770UserId}), S3469(<<"Api.sest">>, 19) end, begin S2771Enc = 'Tianjiupai.Models':'encode_create_user_response'(), begin S2772RespBody = 'SesterlJson.Encode':'run'(S2771Enc(#{user_id => S2770UserId})), sesterl_internal_prim:'return'({'ok', {S2770UserId, S2772RespBody}}) end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end.
'is_existent_user'(S2774UserId) -> 'Tianjiupai.User':'exists'(S2774UserId).
'delete_user'(S2776UserId) -> begin S2777Deleted = 'Tianjiupai.User':'delete'(S2776UserId), case S2777Deleted of true -> begin _ = begin S3472 = 'Tianjiupai.Logger':'info'({"user deleted (user_id: ~s)", 1}, {S2776UserId}), S3472(<<"Api.sest">>, 40) end, sesterl_internal_prim:'return'(ok) end; false -> sesterl_internal_prim:'return'(ok) end end.
'subscribe_plaza'(S2779UserId, S2780WsProc) -> 'Tianjiupai.PlazaServer':'subscribe'(S2779UserId, S2780WsProc).
'get_all_rooms'() -> begin S2782RoomSummaries = 'Tianjiupai.Room':'get_all_rooms'(), begin S2783Enc = 'Tianjiupai.Models':'encode_get_all_rooms_response'(), sesterl_internal_prim:'return'('SesterlJson.Encode':'run'(S2783Enc(#{rooms => S2782RoomSummaries}))) end end.
'get_personal_state'(S2785RoomId, S2786UserId, S2787Validator) -> case S2787Validator(S2786UserId) of true -> begin S2788Res = 'Tianjiupai.Room':'get_personal_state'(S2785RoomId, S2786UserId), case S2788Res of {'ok', S2789PersonalState} -> begin S2790Enc = 'Tianjiupai.Models':'encode_personal_state'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2790Enc(S2789PersonalState))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end.
'submit'(S2792RoomId, S2793UserId, S2794Cards) -> begin S2795Res = 'Tianjiupai.Room':'submit'(S2792RoomId, S2793UserId, S2794Cards), begin S2796IsSuccess = case S2795Res of {'ok', _} -> true; 'error' -> false end, begin _ = begin S3477 = 'Tianjiupai.Logger':'info'({"submit (room_id: ~s, user_id: ~s, cards: ~p, success: ~p)", 4}, {S2793UserId, S2792RoomId, S2794Cards, S2796IsSuccess}), S3477(<<"Api.sest">>, 75) end, case S2795Res of {'ok', {S2797Obs, S2798TrickLastOpt}} -> begin S2799Enc = 'Tianjiupai.Models':'encode_submit_cards_response'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2799Enc(#{new_state => S2797Obs, trick_last => S2798TrickLastOpt}))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end end end.
'create_room'(S2801ReqBody, S2802Validator) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_create_room_request'(), S2801ReqBody) of {'ok', S2803CreateRoomRequest} -> begin S2804UserId = maps:get(user_id, S2803CreateRoomRequest), begin S2805RoomName = maps:get(room_name, S2803CreateRoomRequest), case S2802Validator(S2804UserId) of true -> begin S2806Res = 'Tianjiupai.Room':'create'(S2804UserId, S2805RoomName), case S2806Res of {'ok', S2807RoomId} -> begin _ = begin S3479 = 'Tianjiupai.Logger':'info'({"room created (room_name: ~s, room_id: ~s, created_by: ~s)", 3}, {S2805RoomName, S2807RoomId, S2804UserId}), S3479(<<"Api.sest">>, 98) end, begin S2808Enc = 'Tianjiupai.Models':'encode_create_room_response'(), begin S2809RespBody = 'SesterlJson.Encode':'run'(S2808Enc(#{room_id => S2807RoomId})), sesterl_internal_prim:'return'({'ok', {S2807RoomId, S2809RespBody}}) end end end; {'error', S2810Reason} -> begin _ = begin S3480 = 'Tianjiupai.Logger':'info'({"failed to create room (room_name: ~s, created_by: ~s, reason: ~p)", 3}, {S2805RoomName, S2804UserId, S2810Reason}), S3480(<<"Api.sest">>, 108) end, sesterl_internal_prim:'return'('error') end end end; false -> sesterl_internal_prim:'return'('error') end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end.
'enter_room'(S2812UserId, S2813RoomId, S2814WsProc) -> begin S2815Res = 'Tianjiupai.User':'set_room'(S2812UserId, {'ok', S2813RoomId}), case S2815Res of {'ok', ok} -> begin {'ok', S2816UserName} = 'Tianjiupai.User':'get_name'(S2812UserId), begin S2817User = #{user_id => S2812UserId, user_name => S2816UserName}, begin S2818Res = 'Tianjiupai.Room':'attend'(S2813RoomId, S2817User, S2814WsProc), begin ok = 'Tianjiupai.PlazaServer':'unsubscribe'(S2812UserId), begin S2819IsSuccess = case S2818Res of {'ok', _} -> true; 'error' -> false end, begin _ = begin S3482 = 'Tianjiupai.Logger':'info'({"attend (user_id: ~s, room_id: ~s, success: ~p)", 3}, {S2812UserId, S2813RoomId, S2819IsSuccess}), S3482(<<"Api.sest">>, 136) end, case S2818Res of {'ok', S2820PersonalState} -> begin S2821Enc = 'Tianjiupai.Models':'encode_enter_room_response'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2821Enc(S2820PersonalState))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end end end end end end; {'error', S2822Reason} -> begin _ = begin S3483 = 'Tianjiupai.Logger':'info'({"failed to attend (user_id: ~s, room_id: ~s, reason: ~p)", 3}, {S2812UserId, S2813RoomId, S2822Reason}), S3483(<<"Api.sest">>, 151) end, sesterl_internal_prim:'return'('error') end end end.
'exit_room'(S2824UserId, S2825RoomId, S2826WsProc) -> begin S2827Res = 'Tianjiupai.User':'set_room'(S2824UserId, 'error'), case S2827Res of {'ok', ok} -> begin S2828Res = 'Tianjiupai.Room':'exit'(S2825RoomId, S2824UserId), begin ok = 'Tianjiupai.PlazaServer':'subscribe'(S2824UserId, S2826WsProc), begin S2829IsSuccess = case S2828Res of {'ok', _} -> true; 'error' -> false end, begin _ = begin S3485 = 'Tianjiupai.Logger':'info'({"exit (user_id: ~s, room_id: ~s, success: ~p)", 3}, {S2824UserId, S2825RoomId, S2829IsSuccess}), S3485(<<"Api.sest">>, 172) end, case S2828Res of {'ok', ok} -> begin S2830Enc = 'Tianjiupai.Models':'encode_exit_room_response'(), sesterl_internal_prim:'return'({'ok', 'SesterlJson.Encode':'run'(S2830Enc(ok))}) end; 'error' -> sesterl_internal_prim:'return'('error') end end end end end; {'error', S2831Reason} -> begin _ = begin S3486 = 'Tianjiupai.Logger':'info'({"failed to exit (user_id: ~s, room_id: ~s, reason: ~p)", 3}, {S2824UserId, S2825RoomId, S2831Reason}), S3486(<<"Api.sest">>, 186) end, sesterl_internal_prim:'return'('error') end end end.
'update_room'(S2833RoomId, S2834ReqBody, S2835Validator) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_room_request'(), S2834ReqBody) of {'ok', S2836RoomRequest} -> case S2836RoomRequest of {'room_request_to_enter_room', S2837EnterRoomRequest} -> begin S2838UserId = maps:get(user_id, S2837EnterRoomRequest), case S2835Validator(S2838UserId) of true -> begin S2839Opt = 'Tianjiupai.WebSocketHandler':'where_is'(S2838UserId), case S2839Opt of {'ok', S2840WsProc} -> 'Tianjiupai.Api':'enter_room'(S2838UserId, S2833RoomId, S2840WsProc); 'error' -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end end; {'room_request_to_exit_room', S2841ExitRoomRequest} -> begin S2842UserId = maps:get(user_id, S2841ExitRoomRequest), case S2835Validator(S2842UserId) of true -> begin S2843Opt = 'Tianjiupai.WebSocketHandler':'where_is'(S2842UserId), case S2843Opt of {'ok', S2844WsProc} -> 'Tianjiupai.Api':'exit_room'(S2842UserId, S2833RoomId, S2844WsProc); 'error' -> sesterl_internal_prim:'return'('error') end end; false -> sesterl_internal_prim:'return'('error') end end; {'room_request_to_submit_cards', S2845SubmitCardsRequest} -> begin S2846UserId = maps:get(user_id, S2845SubmitCardsRequest), begin S2847Cards = maps:get(cards, S2845SubmitCardsRequest), case S2835Validator(S2846UserId) of true -> 'Tianjiupai.Api':'submit'(S2833RoomId, S2846UserId, S2847Cards); false -> sesterl_internal_prim:'return'('error') end end end end; {'error', _} -> sesterl_internal_prim:'return'('error') end.
'send_chat'(S2849UserId, S2850Text) -> begin S2851UserStateOpt = 'Tianjiupai.User':'get_info'(S2849UserId), case S2851UserStateOpt of {'error', S2852Reason} -> begin _ = begin S3489 = 'Tianjiupai.Logger':'info'({"failed to send chat (user_id: ~s, reason: ~p)", 2}, {S2849UserId, S2852Reason}), S3489(<<"Api.sest">>, 236) end, sesterl_internal_prim:'return'('error') end; {'ok', S2853UserState} -> case maps:get(belongs_to, S2853UserState) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2854RoomId} -> begin S2855User = #{user_id => S2849UserId, user_name => maps:get(user_name, S2853UserState)}, 'Tianjiupai.Room':'send_chat'(S2854RoomId, S2855User, S2850Text) end end end end.
'ack'(S2857UserId, S2858SnapshotId) -> begin S2859RoomIdOptRes = 'Tianjiupai.User':'get_room'(S2857UserId), case S2859RoomIdOptRes of {'error', S2860Reason} -> begin _ = begin S3491 = 'Tianjiupai.Logger':'warning'({"ack failed (user_id: ~s, snapshot_id: ~s, reason: ~p)", 3}, {S2857UserId, S2858SnapshotId, S2860Reason}), S3491(<<"Api.sest">>, 255) end, sesterl_internal_prim:'return'(ok) end; {'ok', 'error'} -> begin _ = begin S3492 = 'Tianjiupai.Logger':'warning'({"ack failed (user_id: ~s, snapshot_id: ~s)", 2}, {S2857UserId, S2858SnapshotId}), S3492(<<"Api.sest">>, 262) end, sesterl_internal_prim:'return'(ok) end; {'ok', {'ok', S2861RoomId}} -> begin ok = 'Tianjiupai.Room':'ack'(S2861RoomId, S2857UserId, S2858SnapshotId), begin _ = begin S3493 = 'Tianjiupai.Logger':'debug'({"ack (user_id: ~s, snapshot_id: ~s)", 2}, {S2857UserId, S2858SnapshotId}), S3493(<<"Api.sest">>, 267) end, sesterl_internal_prim:'return'(ok) end end end end.
'require_next_inning'(S2863UserId, S2864SnapshotId) -> begin S2865RoomIdOptRes = 'Tianjiupai.User':'get_room'(S2863UserId), case S2865RoomIdOptRes of {'error', S2866Reason} -> begin _ = begin S3495 = 'Tianjiupai.Logger':'warning'({"failed to require next inning (user_id: ~s, snapshot_id: ~s, reason: ~p)", 3}, {S2863UserId, S2864SnapshotId, S2866Reason}), S3495(<<"Api.sest">>, 276) end, sesterl_internal_prim:'return'(ok) end; {'ok', 'error'} -> begin _ = begin S3496 = 'Tianjiupai.Logger':'warning'({"failed to require next inning (user_id: ~s, snapshot_id: ~s)", 2}, {S2863UserId, S2864SnapshotId}), S3496(<<"Api.sest">>, 284) end, sesterl_internal_prim:'return'(ok) end; {'ok', {'ok', S2867RoomId}} -> 'Tianjiupai.Room':'require_next_inning'(S2867RoomId, S2863UserId, S2864SnapshotId) end end.
'perform_command'(S2869UserId, S2870Data) -> case 'SesterlJson.Decode':'run'('Tianjiupai.Models':'decode_command'(), S2870Data) of {'ok', S2871Command} -> case S2871Command of {'command_comment', S2872Text} -> begin S2873Res = 'Tianjiupai.Api':'send_chat'(S2869UserId, S2872Text), case S2873Res of {'ok', ok} -> sesterl_internal_prim:'return'(ok); 'error' -> begin _ = begin S3498 = 'Tianjiupai.Logger':'warning'({"failed to send a chat comment (user_id: ~s, text: ~s)", 2}, {S2869UserId, S2872Text}), S3498(<<"Api.sest">>, 306) end, sesterl_internal_prim:'return'(ok) end end end; {'command_ack', S2874SnapshotId} -> 'Tianjiupai.Api':'ack'(S2869UserId, S2874SnapshotId); 'command_heartbeat' -> sesterl_internal_prim:'return'(ok); {'command_next_inning', S2875SnapshotId} -> begin S2876Res = 'Tianjiupai.Api':'require_next_inning'(S2869UserId, S2875SnapshotId), begin _ = begin S3499 = 'Tianjiupai.Logger':'debug'({"next inning (user_id: ~s, snapshot_id: ~s, res: ~p)", 3}, {S2869UserId, S2875SnapshotId, S2876Res}), S3499(<<"Api.sest">>, 323) end, sesterl_internal_prim:'return'(ok) end end end; {'error', _} -> sesterl_internal_prim:'return'(ok) end.
'set_websocket_connection'(S2878UserId, S2879WsProc) -> begin S2880RoomOptRes = 'Tianjiupai.User':'get_room'(S2878UserId), case S2880RoomOptRes of {'error', S2881Reason} -> begin _ = begin S3501 = 'Tianjiupai.Logger':'warning'({"failed to set websocket connection (user_id: ~s, reason: ~p)", 2}, {S2878UserId, S2881Reason}), S3501(<<"Api.sest">>, 340) end, sesterl_internal_prim:'return'(ok) end; {'ok', {'ok', S2882RoomId}} -> begin _ = 'Tianjiupai.Room':'set_connection'(S2882RoomId, S2878UserId, S2879WsProc), sesterl_internal_prim:'return'(ok) end; {'ok', 'error'} -> begin _ = 'Tianjiupai.Api':'subscribe_plaza'(S2878UserId, S2879WsProc), sesterl_internal_prim:'return'(ok) end end end.
'encode_notification'(S2884Notification) -> begin S2885Enc = 'Tianjiupai.Models':'encode_notification'(), 'SesterlJson.Encode':'run'(S2885Enc(S2884Notification)) end.
'make_flag_user'(S2887UserIdOpt) -> begin S2888Enc = 'Tianjiupai.Models':'encode_flag_user_option'(), begin S2893Opt = case S2887UserIdOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2889UserId} -> begin S2890Res = 'Tianjiupai.User':'get_info'(S2889UserId), case S2890Res of {'ok', S2891Info} -> sesterl_internal_prim:'return'({'ok', #{belongs_to => maps:get(belongs_to, S2891Info), id => S2889UserId, name => maps:get(user_name, S2891Info)}}); {'error', S2892Reason} -> begin _ = begin S3504 = 'Tianjiupai.Logger':'warning'({"failed to make flag_user (user_id: ~s, reason: ~p)", 2}, {S2889UserId, S2892Reason}), S3504(<<"Api.sest">>, 375) end, sesterl_internal_prim:'return'('error') end end end end, sesterl_internal_prim:'return'('SesterlJson.Encode':'run'(S2888Enc(S2893Opt))) end end.
