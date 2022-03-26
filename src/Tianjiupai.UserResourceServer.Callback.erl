-module('Tianjiupai.UserResourceServer.Callback').
-export(['init'/1, 'handle_call'/3, 'handle_cast'/2, 'handle_down'/4, 'handle_timeout'/1, 'handle_info'/2, 'terminate'/2]).
'init'(ok) -> begin S2560State = #{num_users => 0}, 'SesterlStdlib.GenServer':'init_ok'(S2560State) end.
'handle_call'(S2562Req, _, S2563State) -> case S2562Req of {'add_user', S2564UserId, S2565UserName} -> begin S2566NumUsers = maps:get(num_users, S2563State), case (S2566NumUsers >= 'Tianjiupai.Constants':'maximum_num_users'()) of true -> 'SesterlStdlib.GenServer':'reply'({'user_added', {'error', 'SesterlStdlib.RawValue':'forget'(<<"capacity exceeded"/utf8>>)}}, S2563State); false -> begin S2567Res = 'Tianjiupai.UserServerSup':'start_child'(S2564UserId, S2565UserName), case S2567Res of {'ok', S2568Proc} -> begin S2569Mref = 'SesterlStdlib.MonitorRef':'monitor'('Tianjiupai.UserServer':'as_pid'(S2568Proc)), begin _ = begin S3400 = 'Tianjiupai.Logger':'info'({"increment num_users ~p --> ~p (proc: ~p, mref: ~p)", 4}, {S2566NumUsers, (S2566NumUsers + 1), S2568Proc, S2569Mref}), S3400(<<"UserResourceServer.sest">>, 61) end, 'SesterlStdlib.GenServer':'reply'({'user_added', {'ok', S2568Proc}}, #{num_users => (S2566NumUsers + 1)}) end end; {'error', S2570Err} -> 'SesterlStdlib.GenServer':'reply'({'user_added', {'error', S2570Err}}, S2563State) end end end end; 'get_num_users' -> 'SesterlStdlib.GenServer':'reply'({'num_users_got', maps:get(num_users, S2563State)}, S2563State) end.
'handle_cast'(S2572Msg, S2573State) -> begin _ = begin S3402 = 'Tianjiupai.Logger':'warning'({"unexpected cast (message: ~p, state: ~p)", 2}, {S2572Msg, S2573State}), S3402(<<"UserResourceServer.sest">>, 75) end, 'SesterlStdlib.GenServer':'no_reply'(S2573State) end.
'handle_down'(S2575Mref, S2576Pid, S2577Reason, S2578State) -> begin S2579NumUsers = maps:get(num_users, S2578State), begin _ = begin S3404 = 'Tianjiupai.Logger':'info'({"decrement num_users ~p --> ~p (pid: ~p, mref: ~p, reason: ~p)", 5}, {S2579NumUsers, (S2579NumUsers - 1), S2576Pid, S2575Mref, S2577Reason}), S3404(<<"UserResourceServer.sest">>, 81) end, 'SesterlStdlib.GenServer':'no_reply'(#{num_users => (S2579NumUsers - 1)}) end end.
'handle_timeout'(S2581State) -> 'SesterlStdlib.GenServer':'no_reply'(S2581State).
'handle_info'(S2583Info, S2584State) -> begin _ = begin S3407 = 'Tianjiupai.Logger':'warning'({"unexpected info (info: ~p, state: ~p)", 2}, {S2583Info, S2584State}), S3407(<<"UserResourceServer.sest">>, 91) end, 'SesterlStdlib.GenServer':'no_reply'(S2584State) end.
'terminate'(S2586Reason, S2587State) -> begin _ = begin S3409 = 'Tianjiupai.Logger':'warning'({"terminate (reason: ~p, state: ~p)", 2}, {S2586Reason, S2587State}), S3409(<<"UserResourceServer.sest">>, 95) end, sesterl_internal_prim:'return'(ok) end.
