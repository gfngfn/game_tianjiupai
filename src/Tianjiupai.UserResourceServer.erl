-module('Tianjiupai.UserResourceServer').
-export(['start_link'/0, 'as_pid'/1, 'add'/2, 'get_number_of_users'/0]).
'start_link'() -> 'Tianjiupai.UserResourceServer.Impl':'start_link_name'(ok, {'global', ok}).
'as_pid'(S2642Proc) -> 'Tianjiupai.UserResourceServer.Impl':'as_pid'(S2642Proc).
'add'(S2644UserId, S2645UserName) -> begin S2646ProcRes = 'Tianjiupai.UserResourceServer.Impl':'where_is_global'(ok), case S2646ProcRes of {'ok', S2647Proc} -> begin S2648CallRes = 'Tianjiupai.UserResourceServer.Impl':'call'(S2647Proc, {'add_user', S2644UserId, S2645UserName}), case S2648CallRes of {'ok', {'user_added', S2649Res}} -> sesterl_internal_prim:'return'(S2649Res); {'error', S2650Err} -> sesterl_internal_prim:'return'({'error', S2650Err}) end end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"no user resource server"/utf8>>)}) end end.
'get_number_of_users'() -> begin S2652ProcRes = 'Tianjiupai.UserResourceServer.Impl':'where_is_global'(ok), case S2652ProcRes of {'ok', S2653Proc} -> begin S2654CallRes = 'Tianjiupai.UserResourceServer.Impl':'call'(S2653Proc, 'get_num_users'), case S2654CallRes of {'ok', {'num_users_got', S2655N}} -> sesterl_internal_prim:'return'({'ok', S2655N}); {'error', S2656Err} -> sesterl_internal_prim:'return'({'error', S2656Err}) end end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"no user resource server"/utf8>>)}) end end.
