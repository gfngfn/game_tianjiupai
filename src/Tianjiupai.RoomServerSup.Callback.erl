-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2173SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2174ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2173SupFlags, S2174ChildSpec) end end.
