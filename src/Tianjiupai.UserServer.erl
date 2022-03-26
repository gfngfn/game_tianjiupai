-module('Tianjiupai.UserServer').
-export(['get_proc'/1, 'exists'/1, 'delete'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_user_state'/1, 'get_name'/1, 'get_room'/1, 'set_room'/2, 'create_room'/3]).
'get_proc'(S2464UserId) -> 'Tianjiupai.UserServer.Impl':'where_is_global'(S2464UserId).
'exists'(S2466UserId) -> begin S2467Opt = 'Tianjiupai.UserServer':'get_proc'(S2466UserId), case S2467Opt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', _} -> sesterl_internal_prim:'return'(true) end end.
'delete'(S2469UserId) -> begin S2470Opt = 'Tianjiupai.UserServer':'get_proc'(S2469UserId), case S2470Opt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', S2471Proc} -> begin ok = 'Tianjiupai.UserServer.Impl':'cast'(S2471Proc, 'delete_user'), sesterl_internal_prim:'return'(true) end end end.
'start_link'(S2473UserId, S2474UserName) -> 'Tianjiupai.UserServer.Impl':'start_link_name'({S2473UserId, S2474UserName}, {'global', S2473UserId}).
'as_pid'(S2476Proc) -> 'Tianjiupai.UserServer.Impl':'as_pid'(S2476Proc).
'from_pid'(S2478Pid) -> 'Tianjiupai.UserServer.Impl':'from_pid'(S2478Pid).
'call'(S2480UserId, S2481Req, S2482F) -> begin S2483ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2480UserId), case S2483ProcOpt of 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'({<<"no proc"/utf8>>, S2480UserId})}); {'ok', S2484Proc} -> begin S2485RespResult = 'Tianjiupai.UserServer.Impl':'call'(S2484Proc, S2481Req), case S2485RespResult of {'ok', S2486Resp} -> S2482F(S2486Resp); {'error', S2487Err} -> begin _ = begin S3384 = 'Tianjiupai.Logger':'warning'({"call failed (user_id: ~s, req: ~p, proc: ~p, error: ~p)", 4}, {S2480UserId, S2481Req, S2484Proc, S2487Err}), S3384(<<"UserServer.sest">>, 277) end, sesterl_internal_prim:'return'({'error', S2487Err}) end end end end end.
'get_user_state'(S2489UserId) -> 'Tianjiupai.UserServer':'call'(S2489UserId, 'get_user_state', fun({'user_state_got', S2490UserState}) -> sesterl_internal_prim:'return'({'ok', S2490UserState}) end).
'get_name'(S2492UserId) -> begin S2493Res = 'Tianjiupai.UserServer':'get_user_state'(S2492UserId), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S2494U) -> maps:get(user_name, S2494U) end, S2493Res)) end.
'get_room'(S2496UserId) -> begin S2497Res = 'Tianjiupai.UserServer':'get_user_state'(S2496UserId), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'(fun(S2498U) -> maps:get(belongs_to, S2498U) end, S2497Res)) end.
'set_room'(S2500UserId, S2501RoomIdOpt) -> 'Tianjiupai.UserServer':'call'(S2500UserId, {'set_room', S2501RoomIdOpt}, fun({'room_set', S2502Success}) -> case S2502Success of true -> sesterl_internal_prim:'return'({'ok', ok}); false -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"set room failed"/utf8>>)}) end end).
'create_room'(S2504UserId, S2505RoomId, S2506RoomName) -> 'Tianjiupai.UserServer':'call'(S2504UserId, {'create_room', S2505RoomId, S2506RoomName}, fun({'room_created', S2507Res}) -> sesterl_internal_prim:'return'(S2507Res) end).
