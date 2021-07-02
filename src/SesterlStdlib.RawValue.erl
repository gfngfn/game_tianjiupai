-module('SesterlStdlib.RawValue').
-export(['show'/1, 'lift'/1, 'forget'/1]).

    show(Term) ->
        erlang:list_to_binary(lists:flatten(io_lib:format("~w", [Term]))).
  

    lift(N) when erlang:is_integer(N) ->
        {int, N};
    lift(X) when erlang:is_float(X) ->
        {float, X};
    lift(Atom) when erlang:is_atom(Atom) ->
        {atom, erlang:atom_to_binary(Atom)};
    lift(Bin) when erlang:is_binary(Bin) ->
        {binary, Bin};
    lift(Tuple) when erlang:is_tuple(Tuple) ->
        Terms = erlang:tuple_to_list(Tuple),
        {tuple, Terms};
    lift([]) ->
        nil;
    lift([Term1 | Term2]) ->
        {cons, Term1, Term2};
    lift(Map) when erlang:is_map(Map) ->
        Bindings = maps:to_list(Map),
        {map, Bindings};
    lift(Fun) when erlang:is_function(Fun) ->
        {'fun', ?MODULE:show(Fun)};
    lift(Pid) when erlang:is_pid(Pid) ->
        {pid, ?MODULE:show(Pid)};
    lift(Port) when erlang:is_port(Port) ->
        {port, ?MODULE:show(Port)};
    lift(Ref) when erlang:is_reference(Ref) ->
        {ref, ?MODULE:show(Ref)};
    lift(Other) ->
        {other, ?MODULE:show(Other)}.
  

    forget(X) ->
        X.
  
