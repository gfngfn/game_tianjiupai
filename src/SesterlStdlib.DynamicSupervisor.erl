-module('SesterlStdlib.DynamicSupervisor').
-export(['make_child_proc'/1, 'make_child_spec'/1, 'make_child_spec'/2, 'make_sup_flags'/0, 'make_sup_flags'/1, 'init_ok'/2]).

    make_child_proc(PidF) ->
        % io:format("debug L('o' )J make_child_proc, before~n"),
        Result = PidF(),
        % io:format("debug L('o' )J make_child_proc, after~n  ~p~n", [Result]),
        Result.
  
'make_child_spec'(S141Start) -> ?MODULE:'make_child_spec'(S141Start, #{}).
'make_child_spec'(S141Start, S1366) -> S142Restart = sesterl_internal_prim:decode_option_with_default(S1366, restart, fun() -> 'permanent' end), S143ShutdownOpt = sesterl_internal_prim:decode_option(S1366, shutdown), S144Typ = sesterl_internal_prim:decode_option_with_default(S1366, type, fun() -> 'worker' end), begin S146Shutdown = case {S143ShutdownOpt, S144Typ} of {{'ok', S145Shutdown}, _} -> S145Shutdown; {'error', 'worker'} -> {'timeout', 5000}; {'error', 'supervisor'} -> 'infinity' end, #{restart => S142Restart, shutdown => S146Shutdown, start => S141Start, typ => S144Typ} end.
'make_sup_flags'() -> ?MODULE:'make_sup_flags'(#{}).
'make_sup_flags'(S1367) -> S148Intensity = sesterl_internal_prim:decode_option_with_default(S1367, intensity, fun() -> 1 end), S149Period = sesterl_internal_prim:decode_option_with_default(S1367, period, fun() -> 5 end), #{intensity => S148Intensity, period => S149Period}.
'init_ok'(S151SupFlags, S152ChildSpec) -> sesterl_internal_prim:'return'({S151SupFlags, S152ChildSpec}).
