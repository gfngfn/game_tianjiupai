-module('Tianjiupai.RoomServerSup.Callback').
-export(['init'/1]).
'init'(S1029InitArg) -> begin S1030SupFlags = 'SesterlStdlib.DynamicSupervisor':'make_sup_flags'(), begin S1031ChildSpec = 'SesterlStdlib.DynamicSupervisor':'make_child_spec'({'Tianjiupai.RoomServerSup', 'start_child_impl', []}), 'SesterlStdlib.DynamicSupervisor':'init_ok'(S1030SupFlags, S1031ChildSpec) end end.
