-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(S2168InitArg) -> begin S2169SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2170ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2169SupFlags, S2170ChildSpec) end end.
