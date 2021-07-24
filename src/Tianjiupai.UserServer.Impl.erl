-module('Tianjiupai.UserServer.Impl').
-behaviour(gen_server).
-export(['init_impl'/1, 'init'/1, 'handle_call_impl'/3, 'handle_call'/3, 'handle_cast_impl'/2, 'handle_cast'/2, 'handle_info_impl'/2, 'handle_info'/2, 'terminate_impl'/1, 'terminate'/2, 'call'/2, 'call'/3, 'cast'/2, 'as_pid'/1, 'from_pid'/1, 'start_link'/1, 'start_link_name'/2, 'where_is_local'/1, 'where_is_global'/1, 'stop'/1, 'send_info'/2]).
'init_impl'(S2083Arg) -> 'Tianjiupai.UserServer.Callback':'init'(S2083Arg).
      init(Args) ->
          case ?MODULE:init_impl(Args) of
              {ok, State}     -> {ok, State};
              {error, Reason} -> {stop, Reason}
          end.
    
'handle_call_impl'(S2086Req, S2087Pid, S2088State) -> 'Tianjiupai.UserServer.Callback':'handle_call'(S2086Req, S2087Pid, S2088State).
      handle_call(Msg, From, State0) ->
          {Pid, _} = From,
          case ?MODULE:handle_call_impl(Msg, Pid, State0) of
              {reply_impl, Response, StateF} ->
                  State1 = StateF(),
                  {reply, Response, State1};
              {reply_and_stop, Reason, Response, State2} ->
                  {stop, Reason, Response, State2}
          end.
    
'handle_cast_impl'(S2091Msg, S2092State) -> 'Tianjiupai.UserServer.Callback':'handle_cast'(S2091Msg, S2092State).
      handle_cast(Msg, State0) ->
          case ?MODULE:handle_cast_impl(Msg, State0) of
              {no_reply_impl, State1} ->
                  {noreply, State1};
              {stop, Reason, State2} ->
                  {stop, Reason, State2}
          end.
    
'handle_info_impl'(S2095Info, S2096State) -> 'Tianjiupai.UserServer.Callback':'handle_info'(S2095Info, S2096State).
      handle_info(Info, State0) ->
          case handle_info_impl(Info, State0) of
              {no_reply_impl, State1} ->
                  {noreply, State1}
          end.
    
'terminate_impl'(S2099State) -> 'Tianjiupai.UserServer.Callback':'terminate'(S2099State).
      terminate(_Reason, State) ->
          terminate_impl(State).
    
      call(Pid, Msg) ->
          gen_server:call(Pid, Msg).

      call(Pid, Msg, Options) ->
          case maps:find(timeout, Options) of
              {ok, Timeout} -> gen_server:call(Pid, Msg, Timeout);
              error         -> gen_server:call(Pid, Msg)
          end.
    
      cast(Pid, Msg) ->
          gen_server:cast(Pid, Msg).
    
'as_pid'(S2104Proc) -> S2104Proc.
'from_pid'(S2106Pid) -> S2106Pid.
      start_link(Args) ->
          Result = gen_server:start_link(?MODULE, Args, []),
          % io:format("debug L('o' )J returns: ~p~n", [Result]),
          Result.
    
      start_link_name(Args, NameImpl) ->
          % io:format("debug L('o' )J (~p) GS start_link_name (pre):~n  ~p~n", [?MODULE, Args]),
          Name =
              case NameImpl of
                  {local, Bin} -> {local, erlang:binary_to_atom(Bin, utf8)};
                  {global, X}  -> {global, {?MODULE, X}}
              end,
          % io:format("debug L('o' )J (~p) GS start_link_name (arg):~n  ~p~n", [?MODULE, Args]),
          Result = gen_server:start_link(Name, ?MODULE, Args, []),
          % io:format("debug L('o' )J (~p) GS start_link_name (ret):~n  ~p~n", [?MODULE, Result]),
          Result.
    
      where_is_local(NameBin) ->
          NameAtom = erlang:binary_to_atom(NameBin, utf8),
          case erlang:whereis(NameAtom) of
              Pid when is_pid(Pid) -> {ok, Pid};
              _                    -> error
          end.
    
      where_is_global(X) ->
          case global:whereis_name({?MODULE, X}) of
              Pid when is_pid(Pid) -> {ok, Pid};
              undefined            -> error
          end.
    
      stop(Pid) ->
          gen_server:stop(Pid).
    
'send_info'(S2113Pid, S2114Info) -> sesterl_internal_prim:'send'(S2113Pid, S2114Info).
