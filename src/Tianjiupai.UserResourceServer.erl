-module('Tianjiupai.UserResourceServer').
-export(['start_link'/0, 'as_pid'/1, 'add'/2]).
'start_link'() -> 'Tianjiupai.UserResourceServer.Impl':'start_link_name'(ok, {'global', ok}).
'as_pid'(S2620Proc) -> 'Tianjiupai.UserResourceServer.Impl':'as_pid'(S2620Proc).
'add'(S2622UserId, S2623UserName) -> begin S2624ProcRes = 'Tianjiupai.UserResourceServer.Impl':'where_is_global'(ok), case S2624ProcRes of {'ok', S2625Proc} -> begin S2626CallRes = 'Tianjiupai.UserResourceServer.Impl':'call'(S2625Proc, {'add_user', S2622UserId, S2623UserName}), case S2626CallRes of {'ok', {'user_added', S2627Res}} -> sesterl_internal_prim:'return'(S2627Res); {'error', S2628Err} -> sesterl_internal_prim:'return'({'error', S2628Err}) end end; 'error' -> sesterl_internal_prim:'return'({'error', 'SesterlStdlib.RawValue':'forget'(<<"no user resource server"/utf8>>)}) end end.
