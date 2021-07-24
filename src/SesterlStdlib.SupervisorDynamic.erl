-module('SesterlStdlib.SupervisorDynamic').
-export(['make_child_proc'/1, 'make_child_spec'/1, 'make_child_spec'/2, 'make_sup_flags'/0, 'make_sup_flags'/1, 'init_ok'/2]).
    make_child_proc(PidF) ->
        % io:format("debug L('o' )J make_child_proc, before~n"),
        Result = PidF(),
        % io:format("debug L('o' )J make_child_proc, after~n  ~p~n", [Result]),
        Result.
  
'make_child_spec'(S152Start) -> ?MODULE:'make_child_spec'(S152Start, #{}).
'make_child_spec'(S152Start, S2394) -> S153Restart = sesterl_internal_prim:decode_option_with_default(S2394, restart, fun() -> 'permanent' end), S154ShutdownOpt = sesterl_internal_prim:decode_option(S2394, shutdown), S155Typ = sesterl_internal_prim:decode_option_with_default(S2394, type, fun() -> 'worker' end), begin S157Shutdown = case {S154ShutdownOpt, S155Typ} of {{'ok', S156Shutdown}, _} -> S156Shutdown; {'error', 'worker'} -> {'timeout', 5000}; {'error', 'supervisor'} -> 'infinity' end, #{restart => S153Restart, shutdown => S157Shutdown, start => S152Start, typ => S155Typ} end.
'make_sup_flags'() -> ?MODULE:'make_sup_flags'(#{}).
'make_sup_flags'(S2395) -> S159Intensity = sesterl_internal_prim:decode_option_with_default(S2395, intensity, fun() -> 1 end), S160Period = sesterl_internal_prim:decode_option_with_default(S2395, period, fun() -> 5 end), #{intensity => S159Intensity, period => S160Period}.
'init_ok'(S162SupFlags, S163ChildSpec) -> sesterl_internal_prim:'return'({S162SupFlags, S163ChildSpec}).
