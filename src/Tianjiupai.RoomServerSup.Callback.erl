-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2159SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2160ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2159SupFlags, S2160ChildSpec) end end.
