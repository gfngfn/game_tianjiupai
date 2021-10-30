-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2156SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2157ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2156SupFlags, S2157ChildSpec) end end.
