-module('Tianjiupai.UserServerSup.Callback').
-export(['init'/1]).
'init'(S1246InitArg) -> begin S1247SupFlags = 'SesterlStdlib.DynamicSupervisor':'make_sup_flags'(), begin S1248ChildSpec = 'SesterlStdlib.DynamicSupervisor':'make_child_spec'({'Tianjiupai.UserServerSup', 'start_child_impl', []}), 'SesterlStdlib.DynamicSupervisor':'init_ok'(S1247SupFlags, S1248ChildSpec) end end.
