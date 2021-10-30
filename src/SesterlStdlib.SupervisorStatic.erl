-module('SesterlStdlib.SupervisorStatic').
-export(['make_child_proc'/1, 'make_child_spec'/2, 'make_child_spec'/3, 'make_sup_flags'/0, 'make_sup_flags'/1, 'init_ok'/2]).
    make_child_proc(PidF) ->
        PidF().
  
'make_child_spec'(S136Id, S137Start) -> ?MODULE:'make_child_spec'(S136Id, S137Start, #{}).
'make_child_spec'(S136Id, S137Start, S2655) -> S138Restart = sesterl_internal_prim:decode_option_with_default(S2655, restart, fun() -> 'permanent' end), S139ShutdownOpt = sesterl_internal_prim:decode_option(S2655, shutdown), S140Typ = sesterl_internal_prim:decode_option_with_default(S2655, type, fun() -> 'worker' end), begin S142Shutdown = case {S139ShutdownOpt, S140Typ} of {{'ok', S141Shutdown}, _} -> S141Shutdown; {'error', 'worker'} -> {'timeout', 5000}; {'error', 'supervisor'} -> 'infinity' end, #{id => S136Id, restart => S138Restart, shutdown => S142Shutdown, start => S137Start, typ => S140Typ} end.
'make_sup_flags'() -> ?MODULE:'make_sup_flags'(#{}).
'make_sup_flags'(S2656) -> S145Intensity = sesterl_internal_prim:decode_option_with_default(S2656, intensity, fun() -> 1 end), S146Period = sesterl_internal_prim:decode_option_with_default(S2656, period, fun() -> 5 end), S144Strategy = sesterl_internal_prim:decode_option_with_default(S2656, strategy, fun() -> 'one_for_one' end), #{intensity => S145Intensity, period => S146Period, strategy => S144Strategy}.
'init_ok'(S148SupFlags, S149ChildSpecs) -> sesterl_internal_prim:'return'({S148SupFlags, S149ChildSpecs}).
