-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(S1920InitArg) -> begin S1921SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S1922ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S1921SupFlags, S1922ChildSpec) end end.
