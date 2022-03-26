-module('SesterlStdlib.SupervisorDynamic').
-export(['make_child_proc'/1, 'make_child_spec'/1, 'make_child_spec'/2, 'make_sup_flags'/0, 'make_sup_flags'/1, 'init_ok'/2]).
    make_child_proc(PidF) ->
        % io:format("debug L('o' )J make_child_proc, before~n"),
        Result = PidF(),
        % io:format("debug L('o' )J make_child_proc, after~n  ~p~n", [Result]),
        Result.
  
'make_child_spec'(S102Start) -> ?MODULE:'make_child_spec'(S102Start, #{}).
'make_child_spec'(S102Start, S2977) -> S103Restart = sesterl_internal_prim:decode_option_with_default(S2977, restart, fun() -> 'permanent' end), S104ShutdownOpt = sesterl_internal_prim:decode_option(S2977, shutdown), S105Typ = sesterl_internal_prim:decode_option_with_default(S2977, type, fun() -> 'worker' end), begin S107Shutdown = case {S104ShutdownOpt, S105Typ} of {{'ok', S106Shutdown}, _} -> S106Shutdown; {'error', 'worker'} -> {'timeout', 5000}; {'error', 'supervisor'} -> 'infinity' end, #{restart => S103Restart, shutdown => S107Shutdown, start => S102Start, typ => S105Typ} end.
'make_sup_flags'() -> ?MODULE:'make_sup_flags'(#{}).
'make_sup_flags'(S2978) -> S109Intensity = sesterl_internal_prim:decode_option_with_default(S2978, intensity, fun() -> 1 end), S110Period = sesterl_internal_prim:decode_option_with_default(S2978, period, fun() -> 5 end), #{intensity => S109Intensity, period => S110Period}.
'init_ok'(S112SupFlags, S113ChildSpec) -> sesterl_internal_prim:'return'({S112SupFlags, S113ChildSpec}).
