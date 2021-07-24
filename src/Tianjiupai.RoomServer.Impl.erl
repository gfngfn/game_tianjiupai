-module('Tianjiupai.RoomServer.Impl').
-behaviour(gen_server).
-export(['init_impl'/1, 'init'/1, 'handle_call_impl'/3, 'handle_call'/3, 'handle_cast_impl'/2, 'handle_cast'/2, 'handle_info_impl'/2, 'handle_info'/2, 'terminate_impl'/1, 'terminate'/2, 'call'/2, 'call'/3, 'cast'/2, 'as_pid'/1, 'from_pid'/1, 'start_link'/1, 'start_link_name'/2, 'where_is_local'/1, 'where_is_global'/1, 'stop'/1, 'send_info'/2]).
'init_impl'(S1802Arg) -> 'Tianjiupai.RoomServer.Callback':'init'(S1802Arg).
      init(Args) ->
          case ?MODULE:init_impl(Args) of
              {ok, State}     -> {ok, State};
              {error, Reason} -> {stop, Reason}
          end.
    
'handle_call_impl'(S1805Req, S1806Pid, S1807State) -> 'Tianjiupai.RoomServer.Callback':'handle_call'(S1805Req, S1806Pid, S1807State).
      handle_call(Msg, From, State0) ->
          {Pid, _} = From,
          case ?MODULE:handle_call_impl(Msg, Pid, State0) of
              {reply_impl, Response, StateF} ->
                  State1 = StateF(),
                  {reply, Response, State1};
              {reply_and_stop, Reason, Response, State2} ->
                  {stop, Reason, Response, State2}
          end.
    
'handle_cast_impl'(S1810Msg, S1811State) -> 'Tianjiupai.RoomServer.Callback':'handle_cast'(S1810Msg, S1811State).
      handle_cast(Msg, State0) ->
          case ?MODULE:handle_cast_impl(Msg, State0) of
              {no_reply_impl, State1} ->
                  {noreply, State1};
              {stop, Reason, State2} ->
                  {stop, Reason, State2}
          end.
    
'handle_info_impl'(S1814Info, S1815State) -> 'Tianjiupai.RoomServer.Callback':'handle_info'(S1814Info, S1815State).
      handle_info(Info, State0) ->
          case handle_info_impl(Info, State0) of
              {no_reply_impl, State1} ->
                  {noreply, State1}
          end.
    
'terminate_impl'(S1818State) -> 'Tianjiupai.RoomServer.Callback':'terminate'(S1818State).
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
    
'as_pid'(S1823Proc) -> S1823Proc.
'from_pid'(S1825Pid) -> S1825Pid.
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
    
'send_info'(S1832Pid, S1833Info) -> sesterl_internal_prim:'send'(S1832Pid, S1833Info).
