-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2165SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2166ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2165SupFlags, S2166ChildSpec) end end.
