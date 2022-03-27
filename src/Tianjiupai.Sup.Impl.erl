-module('Tianjiupai.Sup.Impl').
-behaviour(supervisor).
-export(['init_impl'/1, 'init'/1, 'as_pid'/1, 'from_pid'/1, 'start_link'/1, 'start_link_name'/2, 'where_is_local'/1, 'where_is_global'/1]).
'init_impl'(S2913Args) -> 'Tianjiupai.Sup.Callback':'init'(S2913Args).
      init(Args) ->
          {SupFlags, ChildSpecsImpl} = init_impl(Args),
          ChildSpecs =
              lists:map(
                  fun(ChildSpecImpl) ->
                      #{
                          id       := ChildId,
                          start    := StartFunc,
                          restart  := Restart,
                          shutdown := ShutdownImpl,
                          typ      := Worker
                      } = ChildSpecImpl,
                      Shutdown =
                          case ShutdownImpl of
                              brutal_kill  -> brutal_kill;
                              {timeout, N} -> max(N, 0);
                              infinity     -> infinity
                          end,
                      #{
                          id       => ChildId,
                          start    => StartFunc,
                          restart  => Restart,
                          shutdown => Shutdown,
                          type     => Worker
                      }
                  end,
                  ChildSpecsImpl),
          {ok, {SupFlags, ChildSpecs}}.
    
      as_pid(Pid) -> Pid.
    
      from_pid(Pid) -> Pid.
    
      start_link(InitArg) ->
          % io:format("debug L('o' )J (~p) St start_link (pre):~n  ~p~n", [?MODULE, InitArg]),
          % io:format("debug L('o' )J (~p) St start_link (arg):~n  ~p~n", [?MODULE, InitArg]),
          Result = supervisor:start_link(?MODULE, InitArg),
          % io:format("debug L('o' )J (~p) St start_link (ret):~n  ~p~n", [?MODULE, Result]),
          Result.
    
      start_link_name(InitArg, NameImpl) ->
          % io:format("debug L('o' )J (~p) St start_link_name (pre):~n  ~p~n", [?MODULE, InitArg]),
          Name =
              case NameImpl of
                  {local, Bin} -> {local, erlang:binary_to_atom(Bin, utf8)};
                  {global, X}  -> {via, global, {?MODULE, X}}
              end,
          % io:format("debug L('o' )J (~p) St start_link_name (arg):~n  ~p~n", [?MODULE, InitArg]),
          Return = supervisor:start_link(Name, ?MODULE, InitArg),
          % io:format("debug L('o' )J (~p) St start_link_name (ret):~n  ~p~n", [?MODULE, Return]),
          Return.
    
      where_is_local(NameBin) ->
          NameAtom = erlang:binary_to_atom(NameBin, utf8),
          case erlang:whereis(NameAtom) of
              Pid when erlang:is_pid(Pid) -> {ok, Pid};
              _                           -> error
          end.
    
      where_is_global(X) ->
          case global:whereis_name({?MODULE, X}) of
              Pid when erlang:is_pid(Pid) -> {ok, Pid};
              undefined                   -> error
          end.
    
