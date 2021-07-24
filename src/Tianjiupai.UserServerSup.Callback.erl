-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(S2167InitArg) -> begin S2168SupFlags = 'SesterlStdlib.SupervisorDynamic':'make_sup_flags'(), begin S2169ChildSpec = 'SesterlStdlib.SupervisorDynamic':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}), 'SesterlStdlib.SupervisorDynamic':'init_ok'(S2168SupFlags, S2169ChildSpec) end end.
