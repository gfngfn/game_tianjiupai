-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(_) -> begin S2386SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2387ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2386SupFlags, S2387ChildSpec) end end.
