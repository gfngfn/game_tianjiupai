-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2507SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2508ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2507SupFlags, S2508ChildSpec) end end.
