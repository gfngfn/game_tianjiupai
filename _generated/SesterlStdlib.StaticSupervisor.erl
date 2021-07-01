-module('SesterlStdlib.StaticSupervisor').
-export(['make_child_proc'/1, 'make_child_spec'/2, 'make_child_spec'/3, 'make_sup_flags'/0, 'make_sup_flags'/1, 'init_ok'/2]).

    make_child_proc(PidF) ->
        PidF().
  
'make_child_spec'(S170Id, S171Start) -> ?MODULE:'make_child_spec'(S170Id, S171Start, #{}).
'make_child_spec'(S170Id, S171Start, S1369) -> S172Restart = sesterl_internal_prim:decode_option_with_default(S1369, restart, fun() -> 'permanent' end), S173ShutdownOpt = sesterl_internal_prim:decode_option(S1369, shutdown), S174Typ = sesterl_internal_prim:decode_option_with_default(S1369, type, fun() -> 'worker' end), begin S176Shutdown = case {S173ShutdownOpt, S174Typ} of {{'ok', S175Shutdown}, _} -> S175Shutdown; {'error', 'worker'} -> {'timeout', 5000}; {'error', 'supervisor'} -> 'infinity' end, #{id => S170Id, restart => S172Restart, shutdown => S176Shutdown, start => S171Start, typ => S174Typ} end.
'make_sup_flags'() -> ?MODULE:'make_sup_flags'(#{}).
'make_sup_flags'(S1370) -> S179Intensity = sesterl_internal_prim:decode_option_with_default(S1370, intensity, fun() -> 1 end), S180Period = sesterl_internal_prim:decode_option_with_default(S1370, period, fun() -> 5 end), S178Strategy = sesterl_internal_prim:decode_option_with_default(S1370, strategy, fun() -> 'one_for_one' end), #{intensity => S179Intensity, period => S180Period, strategy => S178Strategy}.
'init_ok'(S182SupFlags, S183ChildSpecs) -> sesterl_internal_prim:'return'({S182SupFlags, S183ChildSpecs}).
