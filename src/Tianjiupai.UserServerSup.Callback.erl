-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2528SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2529ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}, #{restart => 'temporary'}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2528SupFlags, S2529ChildSpec) end end.
