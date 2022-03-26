-module('Tianjiupai.UserServerSup.Impl').
-behaviour(supervisor).
-export(['init_impl'/1, 'init'/1, 'as_pid'/1, 'from_pid'/1, 'start_link'/1, 'start_link_name'/2, 'where_is_local'/1, 'where_is_global'/1, 'start_child'/2, 'which_children'/1, 'terminate_child'/2]).
'init_impl'(S2524InitArg) -> 'Tianjiupai.UserServerSup.Callback':'init'(S2524InitArg).
      init(InitArg) ->
          % io:format("debug L('o' )J (~p) init:~n  ~p~n", [?MODULE, InitArg]),
          {SupFlagsImpl, ChildSpecImpl} = init_impl(InitArg),
          #{
              intensity := Intensity,
              period    := Period
          } = SupFlagsImpl,
          SupFlags = #{
              strategy  => simple_one_for_one,
              intensity => erlang:max(Intensity, 0),
              period    => erlang:max(Period, 1)
          },
          #{
              start    := StartFunc,
              restart  := Restart,
              shutdown := ShutdownImpl,
              typ      := Worker
          } = ChildSpecImpl,
          % io:format("debug L('o' )J start func:~n  ~p~n", [StartFunc]),
          Shutdown =
              case ShutdownImpl of
                  brutal_kill  -> brutal_kill;
                  {timeout, N} -> max(N, 0);
                  infinity     -> infinity
              end,
          ChildSpec = #{
              id       => ?MODULE,
              start    => StartFunc,
              restart  => Restart,
              shutdown => Shutdown,
              type     => Worker
          },
          {ok, {SupFlags, [ChildSpec]}}.
    
      as_pid(Pid) -> Pid.
    
      from_pid(Pid) -> Pid.
    
      start_link(InitArg) ->
          case supervisor:start_link(?MODULE, InitArg) of
              {ok, SupPid} when is_pid(SupPid) -> {ok, SupPid};
              {error, _} = Err                 -> Err
          end.
    
      start_link_name(InitArg, NameImpl) ->
          % io:format("debug L('o' )J (~p) start_link_name (pre):~n  ~p~n", [?MODULE, InitArg]),
          Name =
              case NameImpl of
                  {local, Bin} -> {local, erlang:binary_to_atom(Bin, utf8)};
                  {global, X}  -> {via, global, {?MODULE, X}}
              end,
          % io:format("debug L('o' )J (~p) start_link_name (arg):~n  ~p~n", [?MODULE, InitArg]),
          Result = supervisor:start_link(Name, ?MODULE, InitArg),
          % io:format("debug L('o' )J (~p) start_link_name (ret):~n  ~p~n", [?MODULE, Result]),
          case Result of
              {ok, SupPid} when erlang:is_pid(SupPid) -> {ok, SupPid};
              {error, _} = Err                        -> Err
          end.
    
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
    
      start_child(SupPid, StartArg) ->
          % io:format("debug L('o' )J (~p) start_child (arg):~n  ~p~n", [?MODULE, StartArg]),
          Result = supervisor:start_child(SupPid, [StartArg]),
          % io:format("debug L('o' )J (~p) start_child (ret):~n  ~p~n", [?MODULE, Result]),
          case Result of
              {ok, ChildPid} when is_pid(ChildPid) -> {ok, ChildPid};
              {error, _} = Err                     -> Err
          end.
    
      which_children(SupPid) ->
          Children = supervisor:which_children(SupPid),
          lists:filtermap(
              fun({_Id, Pid, _Type, _Modules}) when is_pid(Pid) ->
                      {true, Pid};
                 (_Child) ->
                      false
              end,
              Children).
    
      terminate_child(SupPid, ChildPid) ->
          Result = supervisor:terminate_child(SupPid, ChildPid),
          case Result of
              ok               -> {ok, ok};
              {error, _} = Err -> Err
          end.
    
