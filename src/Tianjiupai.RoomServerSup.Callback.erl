-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(S1921InitArg) -> begin S1922SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S1923ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S1922SupFlags, S1923ChildSpec) end end.
