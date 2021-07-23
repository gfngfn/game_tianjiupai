-module('SesterlStdlib.SupervisorStatic').
-export(['make_child_proc'/1, 'make_child_spec'/2, 'make_child_spec'/3, 'make_sup_flags'/0, 'make_sup_flags'/1, 'init_ok'/2]).
    make_child_proc(PidF) ->
        PidF().
  
'make_child_spec'(S186Id, S187Start) -> ?MODULE:'make_child_spec'(S186Id, S187Start, #{}).
'make_child_spec'(S186Id, S187Start, S2396) -> S188Restart = sesterl_internal_prim:decode_option_with_default(S2396, restart, fun() -> 'permanent' end), S189ShutdownOpt = sesterl_internal_prim:decode_option(S2396, shutdown), S190Typ = sesterl_internal_prim:decode_option_with_default(S2396, type, fun() -> 'worker' end), begin S192Shutdown = case {S189ShutdownOpt, S190Typ} of {{'ok', S191Shutdown}, _} -> S191Shutdown; {'error', 'worker'} -> {'timeout', 5000}; {'error', 'supervisor'} -> 'infinity' end, #{id => S186Id, restart => S188Restart, shutdown => S192Shutdown, start => S187Start, typ => S190Typ} end.
'make_sup_flags'() -> ?MODULE:'make_sup_flags'(#{}).
'make_sup_flags'(S2397) -> S195Intensity = sesterl_internal_prim:decode_option_with_default(S2397, intensity, fun() -> 1 end), S196Period = sesterl_internal_prim:decode_option_with_default(S2397, period, fun() -> 5 end), S194Strategy = sesterl_internal_prim:decode_option_with_default(S2397, strategy, fun() -> 'one_for_one' end), #{intensity => S195Intensity, period => S196Period, strategy => S194Strategy}.
'init_ok'(S198SupFlags, S199ChildSpecs) -> sesterl_internal_prim:'return'({S198SupFlags, S199ChildSpecs}).
