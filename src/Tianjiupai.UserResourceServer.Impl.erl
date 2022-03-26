-module('Tianjiupai.UserResourceServer.Impl').
-behaviour(gen_server).
-export(['init_impl'/1, 'init'/1, 'handle_call_impl'/3, 'handle_call'/3, 'handle_cast_impl'/2, 'arrange_no_reply_result'/1, 'handle_cast'/2, 'handle_info_impl'/2, 'handle_timeout_impl'/1, 'handle_down_impl'/4, 'handle_info'/2, 'terminate_impl'/2, 'terminate'/2, 'call'/2, 'call'/3, 'cast'/2, 'as_pid'/1, 'from_pid'/1, 'start_link'/1, 'start_link_name'/2, 'where_is_local'/1, 'where_is_global'/1, 'stop'/1, 'send_info'/2]).
'init_impl'(S2590Arg) -> 'Tianjiupai.UserResourceServer.Callback':'init'(S2590Arg).
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
    
'handle_call_impl'(S2593Req, S2594Pid, S2595State) -> 'Tianjiupai.UserResourceServer.Callback':'handle_call'(S2593Req, S2594Pid, S2595State).
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
    
'handle_cast_impl'(S2598Msg, S2599State) -> 'Tianjiupai.UserResourceServer.Callback':'handle_cast'(S2598Msg, S2599State).
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
    
'handle_info_impl'(S2603Info, S2604State) -> 'Tianjiupai.UserResourceServer.Callback':'handle_info'(S2603Info, S2604State).
'handle_timeout_impl'(S2606State) -> 'Tianjiupai.UserResourceServer.Callback':'handle_timeout'(S2606State).
'handle_down_impl'(S2608Mref, S2609Pid, S2610Reason, S2611State) -> 'Tianjiupai.UserResourceServer.Callback':'handle_down'(S2608Mref, S2609Pid, S2610Reason, S2611State).
      handle_info(Msg, State0) ->
          Result =
              case Msg of
                  timeout                              -> handle_timeout_impl(State0);
                  {'$sesterl', Info}                   -> handle_info_impl(Info, State0);
                  {'DOWN', MRef, process, Pid, Reason} -> handle_down_impl(MRef, Pid, Reason, State0)
              end,
          ?MODULE:arrange_no_reply_result(Result).
    
'terminate_impl'(S2614Reason, S2615State) -> 'Tianjiupai.UserResourceServer.Callback':'terminate'(S2614Reason, S2615State).
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
    
'as_pid'(S2620Proc) -> S2620Proc.
'from_pid'(S2622Pid) -> S2622Pid.
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
    
'send_info'(S2629Pid, S2630Info) -> sesterl_internal_prim:'send'(S2629Pid, S2630Info).
