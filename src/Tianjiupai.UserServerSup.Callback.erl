-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2520SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2521ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2520SupFlags, S2521ChildSpec) end end.
