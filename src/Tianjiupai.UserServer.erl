-module('Tianjiupai.UserServer').
-export(['get_proc'/1, 'exists'/1, 'start_link'/2, 'as_pid'/1, 'from_pid'/1, 'call'/3, 'get_user_state'/1, 'get_name'/1, 'get_room'/1, 'set_room'/2]).
'get_proc'(S2339UserId) -> 'Tianjiupai.UserServer.Impl':'where_is_global'(S2339UserId).
'exists'(S2341UserId) -> begin S2342Opt = 'Tianjiupai.UserServer':'get_proc'(S2341UserId), case S2342Opt of 'error' -> sesterl_internal_prim:'return'(false); {'ok', _} -> sesterl_internal_prim:'return'(true) end end.
'start_link'(S2344UserId, S2345UserName) -> 'Tianjiupai.UserServer.Impl':'start_link_name'({S2344UserId, S2345UserName}, {'global', S2344UserId}).
'as_pid'(S2347Proc) -> 'Tianjiupai.UserServer.Impl':'as_pid'(S2347Proc).
'from_pid'(S2349Pid) -> 'Tianjiupai.UserServer.Impl':'from_pid'(S2349Pid).
'call'(S2351UserId, S2352Req, S2353F) -> begin S2354ProcOpt = 'Tianjiupai.UserServer':'get_proc'(S2351UserId), case S2354ProcOpt of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S2355Proc} -> begin S2356RespResult = 'Tianjiupai.UserServer.Impl':'call'(S2355Proc, S2352Req), case S2356RespResult of {'ok', S2357Resp} -> S2353F(S2357Resp); {'error', S2358Err} -> begin _ = begin S3030 = 'Tianjiupai.Logger':'warning'({"call failed (user_id: ~s, req: ~p, proc: ~p, error: ~p)", 4}, {S2351UserId, S2352Req, S2355Proc, S2358Err}), S3030(<<"UserServer.sest">>, 223) end, sesterl_internal_prim:'return'('error') end end end end end.
'get_user_state'(S2360UserId) -> 'Tianjiupai.UserServer':'call'(S2360UserId, 'get_user_state', fun({'user_state_got', S2361UserState}) -> sesterl_internal_prim:'return'({'ok', S2361UserState}) end).
'get_name'(S2363UserId) -> begin S2364Opt = 'Tianjiupai.UserServer':'get_user_state'(S2363UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'map'(fun(S2365U) -> maps:get(user_name, S2365U) end, S2364Opt)) end.
'get_room'(S2367UserId) -> begin S2368Opt = 'Tianjiupai.UserServer':'get_user_state'(S2367UserId), sesterl_internal_prim:'return'('SesterlStdlib.Option':'bind'(S2368Opt, fun(S2369U) -> maps:get(belongs_to, S2369U) end)) end.
'set_room'(S2371UserId, S2372RoomIdOpt) -> 'Tianjiupai.UserServer':'call'(S2371UserId, {'set_room', S2372RoomIdOpt}, fun({'room_set', S2373Success}) -> case S2373Success of true -> sesterl_internal_prim:'return'({'ok', ok}); false -> sesterl_internal_prim:'return'('error') end end).
