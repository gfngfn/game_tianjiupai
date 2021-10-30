-module('Tianjiupai.PlazaServer.Impl').
-behaviour(gen_server).
-export(['init_impl'/1, 'init'/1, 'handle_call_impl'/3, 'handle_call'/3, 'handle_cast_impl'/2, 'arrange_no_reply_result'/1, 'handle_cast'/2, 'handle_info_impl'/2, 'handle_timeout_impl'/1, 'handle_down_impl'/4, 'handle_info'/2, 'terminate_impl'/2, 'terminate'/2, 'call'/2, 'call'/3, 'cast'/2, 'as_pid'/1, 'from_pid'/1, 'start_link'/1, 'start_link_name'/2, 'where_is_local'/1, 'where_is_global'/1, 'stop'/1, 'send_info'/2]).
'init_impl'(S1443Arg) -> 'Tianjiupai.PlazaServer.Callback':'init'(S1443Arg).
      init(Args) ->
          case ?MODULE:init_impl(Args) of
              {ok, {State, MaybeTimeout}} ->
                  case MaybeTimeout of
                      error         -> {ok, State};
                      {ok, Timeout} -> {ok, State, Timeout}
                  end;
              {error, Reason} ->
                  {stop, Reason}
          end.
    
'handle_call_impl'(S1446Req, S1447Pid, S1448State) -> 'Tianjiupai.PlazaServer.Callback':'handle_call'(S1446Req, S1447Pid, S1448State).
      handle_call(Msg, From, State0) ->
          {Pid, _} = From,
          case ?MODULE:handle_call_impl(Msg, Pid, State0) of
              {reply_impl, Response, State1, MaybeTimeout} ->
                  case MaybeTimeout of
                      error         -> {reply, Response, State1};
                      {ok, Timeout} -> {reply, Response, State1, Timeout}
                  end;
              {reply_and_stop_impl, Reason, Response, State2} ->
                  {stop, Reason, Response, State2}
          end.
    
'handle_cast_impl'(S1451Msg, S1452State) -> 'Tianjiupai.PlazaServer.Callback':'handle_cast'(S1451Msg, S1452State).
      arrange_no_reply_result(Result) ->
          case Result of
              {no_reply_impl, State1, MaybeTimeout} ->
                  case MaybeTimeout of
                      error         -> {noreply, State1};
                      {ok, Timeout} -> {noreply, State1, Timeout}
                  end;
              {no_reply_and_stop_impl, Reason, State2} ->
                  {stop, Reason, State2}
          end.
    
      handle_cast(Msg, State0) ->
          Result = ?MODULE:handle_cast_impl(Msg, State0),
          ?MODULE:arrange_no_reply_result(Result).
    
'handle_info_impl'(S1456Info, S1457State) -> 'Tianjiupai.PlazaServer.Callback':'handle_info'(S1456Info, S1457State).
'handle_timeout_impl'(S1459State) -> 'Tianjiupai.PlazaServer.Callback':'handle_timeout'(S1459State).
'handle_down_impl'(S1461Mref, S1462Pid, S1463Reason, S1464State) -> 'Tianjiupai.PlazaServer.Callback':'handle_down'(S1461Mref, S1462Pid, S1463Reason, S1464State).
      handle_info(Msg, State0) ->
          Result =
              case Msg of
                  timeout                              -> handle_timeout_impl(State0);
                  {'$sesterl', Info}                   -> handle_info_impl(Info, State0);
                  {'DOWN', MRef, process, Pid, Reason} -> handle_down_impl(MRef, Pid, Reason, State0)
              end,
          ?MODULE:arrange_no_reply_result(Result).
    
'terminate_impl'(S1467Reason, S1468State) -> 'Tianjiupai.PlazaServer.Callback':'terminate'(S1467Reason, S1468State).
      terminate(Reason, State) ->
          terminate_impl(Reason, State).
    
      call(Pid, Msg) ->
          try
              {ok, gen_server:call(Pid, Msg)}
          catch
              Class:Reason ->
                  {error, {Class, Reason}}
          end.

      call(Pid, Msg, Options) ->
          try
              case maps:find(timeout, Options) of
                  {ok, Timeout} -> {ok, gen_server:call(Pid, Msg, Timeout)};
                  error         -> {ok, gen_server:call(Pid, Msg)}
              end
          catch
              Class:Reason ->
                  {error, {Class, Reason}}
          end.
    
      cast(Pid, Msg) ->
          gen_server:cast(Pid, Msg).
    
'as_pid'(S1473Proc) -> S1473Proc.
'from_pid'(S1475Pid) -> S1475Pid.
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
    
'send_info'(S1482Pid, S1483Info) -> sesterl_internal_prim:'send'(S1482Pid, S1483Info).
