-module('Tianjiupai.Sup.Callback').
-export(['start_room_server_sup'/0, 'start_user_server_sup'/0, 'init'/1]).
'start_room_server_sup'() -> 'SesterlStdlib.SupervisorStatic':'make_child_proc'(fun() -> begin S2260Res = 'Tianjiupai.RoomServerSup':'start_link'(), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.RoomServerSup':'as_pid'/1), S2260Res)) end end).
'start_user_server_sup'() -> 'SesterlStdlib.SupervisorStatic':'make_child_proc'(fun() -> begin S2262Res = 'Tianjiupai.UserServerSup':'start_link'(), sesterl_internal_prim:'return'('SesterlStdlib.Result':'map'((fun 'Tianjiupai.UserServerSup':'as_pid'/1), S2262Res)) end end).
'init'(S2264InitArg) -> begin S2265SupFlags = 'SesterlStdlib.SupervisorStatic':'make_sup_flags'(#{strategy => 'one_for_all'}), begin S2266ChildSpecs = ['SesterlStdlib.SupervisorStatic':'make_child_spec'(<<"room_server_sup"/utf8>>, {'Tianjiupai.Sup.Callback', 'start_room_server_sup', []}, #{type => 'supervisor'}) | ['SesterlStdlib.SupervisorStatic':'make_child_spec'(<<"user_server_sup"/utf8>>, {'Tianjiupai.Sup.Callback', 'start_user_server_sup', []}, #{type => 'supervisor'}) | []]], 'SesterlStdlib.SupervisorStatic':'init_ok'(S2265SupFlags, S2266ChildSpecs) end end.
