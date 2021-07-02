-module('Tianjiupai.RoomServer.Callback').
-export(['init'/1, 'get_members_from_state'/1, 'make_whole_room_state'/1, 'find_user_seat'/2, 'the_other_seats'/1, 'the_other_players'/2, 'make_observable_game_state'/2, 'make_observable'/2, 'make_personal_room_state'/2, 'make_initial_sync'/0, 'start_new_inning'/1, 'notify'/2, 'notify_for_each'/2, 'handle_send_chat'/3, 'handle_attend'/2, 'handle_exit'/2, 'make_winner_diff'/1, 'update_score'/2, 'deal_with_inning_end'/3, 'handle_submit'/3, 'handle_ack'/3, 'handle_require_next_inning'/3, 'update_sync'/2, 'handle_call'/3, 'handle_cast'/2, 'handle_info'/2, 'terminate'/1]).
'init'(S677InitArg) -> begin {S678RoomId, S679RoomName} = S677InitArg, begin S680Settings = #{room_id => S678RoomId, room_name => S679RoomName}, 'SesterlStdlib.GenServer':'init_ok'(#{reversed_logs => [], room_state => {'internal_waiting', #{members => []}}, settings => S680Settings}) end end.
'get_members_from_state'(S682RoomState) -> case S682RoomState of {'internal_waiting', S683WaitingState} -> {false, maps:get(members, S683WaitingState)}; {'internal_playing', S684WholeGameState} -> begin S685GamePlayers = 'Tianjiupai.Quad':'to_list'(maps:get(players, maps:get(meta, S684WholeGameState))), begin S687Members = 'SesterlStdlib.List':'map'(fun(S686P) -> maps:get(user, S686P) end, S685GamePlayers), {true, S687Members} end end end.
'make_whole_room_state'(S689State) -> begin S690Settings = maps:get(settings, S689State), begin {S691IsPlaying, S692Members} = 'Tianjiupai.RoomServer.Callback':'get_members_from_state'(maps:get(room_state, S689State)), #{is_playing => S691IsPlaying, members => S692Members, room_id => maps:get(room_id, S690Settings), room_name => maps:get(room_name, S690Settings)} end end.
'find_user_seat'(S694GameState, S695UserId) -> begin S697Opt = 'Tianjiupai.Quad':'find'(fun(S696P) -> 'SesterlStdlib.Binary':'equal'(maps:get(user_id, maps:get(user, S696P)), S695UserId) end, maps:get(players, maps:get(meta, S694GameState))), 'SesterlStdlib.Option':'map'(fun(S698Pair) -> begin {S699Seat, _} = S698Pair, S699Seat end end, S697Opt) end.
'the_other_seats'(S701Seat) -> case S701Seat of 'seat0' -> ['seat1' | ['seat2' | ['seat3' | []]]]; 'seat1' -> ['seat2' | ['seat3' | ['seat0' | []]]]; 'seat2' -> ['seat3' | ['seat0' | ['seat1' | []]]]; 'seat3' -> ['seat0' | ['seat1' | ['seat2' | []]]] end.
'the_other_players'(S703GameState, S704Seat) -> begin {S706U0, S707U1, S708U2, S709U3} = 'Tianjiupai.Quad':'map'(fun(S705P) -> maps:get(user_id, maps:get(user, S705P)) end, maps:get(players, maps:get(meta, S703GameState))), case S704Seat of 'seat0' -> [S707U1 | [S708U2 | [S709U3 | []]]]; 'seat1' -> [S708U2 | [S709U3 | [S706U0 | []]]]; 'seat2' -> [S709U3 | [S706U0 | [S707U1 | []]]]; 'seat3' -> [S706U0 | [S707U1 | [S708U2 | []]]] end end.
'make_observable_game_state'(S711GameState, S712UserId) -> case 'Tianjiupai.RoomServer.Callback':'find_user_seat'(S711GameState, S712UserId) of 'error' -> 'error'; {'ok', S713Seat} -> begin S714Synchronizing = case maps:get(sync, S711GameState) of 'waiting_input' -> false; {'waiting_sync', _} -> true end, begin S717ObservableInning = case maps:get(inning, S711GameState) of {'during_inning', S715Inning} -> {'observable_during_inning', 'Tianjiupai.Inning':'observe'(S713Seat, S715Inning)}; {'inning_end', S716GainsQuad, _} -> {'observable_inning_end', S716GainsQuad} end, {'ok', #{meta => maps:get(meta, S711GameState), observable_inning => S717ObservableInning, snapshot_id => maps:get(snapshot_id, S711GameState), synchronizing => S714Synchronizing}} end end end.
'make_observable'(S719RoomState, S720UserId) -> case S719RoomState of {'internal_waiting', S721WaitingState} -> {'ok', {'waiting', maps:get(members, S721WaitingState)}}; {'internal_playing', S722GameState} -> 'SesterlStdlib.Option':'map'(fun(S723Obs) -> {'playing', S723Obs} end, 'Tianjiupai.RoomServer.Callback':'make_observable_game_state'(S722GameState, S720UserId)) end.
'make_personal_room_state'(S725State, S726UserId) -> begin S727Settings = maps:get(settings, S725State), case 'Tianjiupai.RoomServer.Callback':'make_observable'(maps:get(room_state, S725State), S726UserId) of 'error' -> 'error'; {'ok', S728Observable} -> {'ok', #{logs => 'SesterlStdlib.List':'reverse'(maps:get(reversed_logs, S725State)), observable => S728Observable, room_id => maps:get(room_id, S727Settings), room_name => maps:get(room_name, S727Settings)}} end end.
'make_initial_sync'() -> {false, false, false, false}.
'start_new_inning'(S731GameMeta) -> begin S732NextParentSeat = maps:get(parent_seat, S731GameMeta), begin S733Inning = 'Tianjiupai.Inning':'start'(S732NextParentSeat), begin S734SnapshotId = 'Tianjiupai.SnapshotId':'generate'(), sesterl_internal_prim:'return'(#{inning => {'during_inning', S733Inning}, meta => S731GameMeta, snapshot_id => S734SnapshotId, sync => {'waiting_sync', 'Tianjiupai.RoomServer.Callback':'make_initial_sync'()}}) end end end.

      notify(UserId, Notifications) ->
          case tianjiupai_websocket:notify(UserId, Notifications) of
              ok ->
                  ok;
              {error, Reason} ->
                  io:format("~p, notify_log failed (reason: ~p, to: ~p, notifications: ~p)",
                      [?MODULE, Reason, UserId, Notifications]),
                  ok
          end.
    
'notify_for_each'(S737UserIds, S738Logs) -> 'SesterlStdlib.List':'for_each'(fun(S739UserId) -> 'Tianjiupai.RoomServer.Callback':'notify'(S739UserId, S738Logs) end, S737UserIds).
'handle_send_chat'(S741User, S742Text, S743State) -> begin S744UserId = maps:get(user_id, S741User), begin {_, S745Members} = 'Tianjiupai.RoomServer.Callback':'get_members_from_state'(maps:get(room_state, S743State)), begin S748Dummy = begin S747UserIds = 'SesterlStdlib.List':'map'(fun(S746U) -> maps:get(user_id, S746U) end, S745Members), 'Tianjiupai.RoomServer.Callback':'notify_for_each'(S747UserIds, [{'notify_comment', S741User, S742Text} | []]) end, 'SesterlStdlib.GenServer':'reply'('chat_sent', maps:put(reversed_logs, [{'log_comment', S741User, S742Text} | maps:get(reversed_logs, S743State)], S743State)) end end end.
'handle_attend'(S750User, S751State) -> begin S752UserId = maps:get(user_id, S750User), case maps:get(room_state, S751State) of {'internal_playing', _} -> 'SesterlStdlib.GenServer':'reply'({'attended', 'error'}, S751State); {'internal_waiting', S753WaitingState} -> begin S754Members = maps:get(members, S753WaitingState), case 'SesterlStdlib.List':'any'(fun(S755U) -> 'SesterlStdlib.Binary':'equal'(maps:get(user_id, S755U), S752UserId) end, S754Members) of true -> begin {'ok', S756PersonalState} = 'Tianjiupai.RoomServer.Callback':'make_personal_room_state'(S751State, S752UserId), 'SesterlStdlib.GenServer':'reply'({'attended', {'ok', S756PersonalState}}, S751State) end; false -> begin S757LogEnter = {'log_entered', S750User}, begin S758NotifEnter = {'notify_entered', S750User}, case S754Members of [S759U0 | [S760U1 | [S761U2 | _]]] -> begin S766PlayerQuad = begin S762Player0 = #{score => 0, user => S759U0}, begin S763Player1 = #{score => 0, user => S760U1}, begin S764Player2 = #{score => 0, user => S761U2}, begin S765Player3 = #{score => 0, user => S750User}, {S762Player0, S763Player1, S764Player2, S765Player3} end end end end, begin S767ParentSeat = 'seat0', begin S768GameMeta = #{inning_index => 0, num_consecutives => 1, parent_seat => S767ParentSeat, players => S766PlayerQuad}, begin S769GameState = 'Tianjiupai.RoomServer.Callback':'start_new_inning'(S768GameMeta), begin S772Dummy = 'SesterlStdlib.List':'for_each'(fun(S770U) -> begin {'ok', S771Obs} = 'Tianjiupai.RoomServer.Callback':'make_observable_game_state'(S769GameState, S770U), 'Tianjiupai.RoomServer.Callback':'notify'(S770U, [S758NotifEnter | [{'notify_game_start', S771Obs} | []]]) end end, [maps:get(user_id, S759U0) | [maps:get(user_id, S760U1) | [maps:get(user_id, S761U2) | []]]]), begin S773State = maps:put(reversed_logs, ['log_game_start' | [S757LogEnter | maps:get(reversed_logs, S751State)]], maps:put(room_state, {'internal_playing', S769GameState}, S751State)), begin {'ok', S774PersonalState} = 'Tianjiupai.RoomServer.Callback':'make_personal_room_state'(S773State, S752UserId), 'SesterlStdlib.GenServer':'reply'({'attended', {'ok', S774PersonalState}}, S773State) end end end end end end end; _ -> begin S775WaitingState = maps:put(members, 'SesterlStdlib.List':'append'(S754Members, [S750User | []]), S753WaitingState), begin S777Dummy = 'Tianjiupai.RoomServer.Callback':'notify_for_each'('SesterlStdlib.List':'map'(fun(S776U) -> maps:get(user_id, S776U) end, S754Members), [S758NotifEnter | []]), begin S778State = maps:put(reversed_logs, [S757LogEnter | maps:get(reversed_logs, S751State)], maps:put(room_state, {'internal_waiting', S775WaitingState}, S751State)), begin {'ok', S779PersonalState} = 'Tianjiupai.RoomServer.Callback':'make_personal_room_state'(S778State, S752UserId), 'SesterlStdlib.GenServer':'reply'({'attended', {'ok', S779PersonalState}}, S778State) end end end end end end end end end end end.
'handle_exit'(S781UserId, S782State) -> case maps:get(room_state, S782State) of {'internal_playing', _} -> 'SesterlStdlib.GenServer':'reply'({'exited', false}, S782State); {'internal_waiting', S783WaitingState} -> begin S785Members = 'SesterlStdlib.List':'filter'(fun(S784U) -> 'SesterlStdlib.Bool':'not'('SesterlStdlib.Binary':'equal'(maps:get(user_id, S784U), S781UserId)) end, maps:get(members, S783WaitingState)), begin S786RoomState = {'internal_waiting', maps:put(members, S785Members, S783WaitingState)}, 'SesterlStdlib.GenServer':'reply'({'exited', true}, maps:put(room_state, S786RoomState, S782State)) end end end.
'make_winner_diff'(S788Losers) -> begin S792LoserDiffSum = 'SesterlStdlib.List':'foldl'(fun(S789Acc, S790Loser) -> begin {_, S791LoserDiff} = S790Loser, (S789Acc + S791LoserDiff) end end, 0, S788Losers), (0 - S792LoserDiffSum) end.
'update_score'(S794Players, S795Updates) -> 'SesterlStdlib.List':'foldl'(fun(S796Players, S797Update) -> begin {S798Seat, S799Diff} = S797Update, begin S800Player = 'Tianjiupai.Quad':'access'(S798Seat, S796Players), 'Tianjiupai.Quad':'update'(S798Seat, maps:put(score, (maps:get(score, S800Player) + S799Diff), S800Player), S796Players) end end end, S794Players, S795Updates).
'deal_with_inning_end'(S802GameMeta, S803WinnerSeat, S804GainsQuad) -> begin S805LoserSeats = 'Tianjiupai.RoomServer.Callback':'the_other_seats'(S803WinnerSeat), begin S806NumConsecutives = maps:get(num_consecutives, S802GameMeta), begin S807ParentSeat = maps:get(parent_seat, S802GameMeta), case 'Tianjiupai.Quad':'seat_equal'(S803WinnerSeat, S807ParentSeat) of true -> begin S811Losers = 'SesterlStdlib.List':'map'(fun(S808LoserSeat) -> begin S809NumGains = 'SesterlStdlib.List':'length'('Tianjiupai.Quad':'access'(S808LoserSeat, S804GainsQuad)), begin S810LoserDiff = case (S809NumGains == 0) of true -> ((0 - 5) * (S806NumConsecutives + 1)); false -> ((S809NumGains - 4) * (S806NumConsecutives + 1)) end, {S808LoserSeat, S810LoserDiff} end end end, S805LoserSeats), begin S812WinnerDiff = 'Tianjiupai.RoomServer.Callback':'make_winner_diff'(S811Losers), begin S813Players = 'Tianjiupai.RoomServer.Callback':'update_score'(maps:get(players, S802GameMeta), [{S803WinnerSeat, S812WinnerDiff} | S811Losers]), #{inning_index => maps:get(inning_index, S802GameMeta), num_consecutives => (S806NumConsecutives + 1), parent_seat => S807ParentSeat, players => S813Players} end end end; false -> begin S818Losers = 'SesterlStdlib.List':'map'(fun(S814LoserSeat) -> begin S815NumGains = 'SesterlStdlib.List':'length'('Tianjiupai.Quad':'access'(S814LoserSeat, S804GainsQuad)), begin S817LoserDiff = begin S816Coeff = case 'Tianjiupai.Quad':'seat_equal'(S814LoserSeat, S807ParentSeat) of true -> (S806NumConsecutives + 1); false -> 1 end, case (S815NumGains == 0) of true -> ((0 - 5) * S816Coeff); false -> ((S815NumGains - 4) * S816Coeff) end end, {S814LoserSeat, S817LoserDiff} end end end, S805LoserSeats), begin S819WinnerDiff = 'Tianjiupai.RoomServer.Callback':'make_winner_diff'(S818Losers), begin S820Players = 'Tianjiupai.RoomServer.Callback':'update_score'(maps:get(players, S802GameMeta), [{S803WinnerSeat, S819WinnerDiff} | S818Losers]), #{inning_index => (maps:get(inning_index, S802GameMeta) + 1), num_consecutives => 1, parent_seat => S803WinnerSeat, players => S820Players} end end end end end end end.
'handle_submit'(S822UserId, S823Cards, S824State) -> begin S849Opt = case maps:get(room_state, S824State) of {'internal_waiting', _} -> sesterl_internal_prim:'return'('error'); {'internal_playing', S825GameState} -> case 'Tianjiupai.RoomServer.Callback':'find_user_seat'(S825GameState, S822UserId) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S826Seat} -> case maps:get(inning, S825GameState) of {'inning_end', _, _} -> sesterl_internal_prim:'return'('error'); {'during_inning', S827Inning} -> case 'Tianjiupai.Inning':'submit'(S826Seat, S823Cards, S827Inning) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', {S828SubmitResult, S829IsFront}} -> begin S830SnapshotId = 'Tianjiupai.SnapshotId':'generate'(), begin S831GameMeta = maps:get(meta, S825GameState), begin {S839GameState, S840LastTableOpt} = case S828SubmitResult of {'continues', S832Inning} -> {#{inning => {'during_inning', S832Inning}, meta => S831GameMeta, snapshot_id => S830SnapshotId, sync => {'waiting_sync', 'Tianjiupai.RoomServer.Callback':'make_initial_sync'()}}, 'error'}; {'wins_trick', S833WinnerSeat, S834LastTable, S835Inning} -> {#{inning => {'during_inning', S835Inning}, meta => S831GameMeta, snapshot_id => S830SnapshotId, sync => {'waiting_sync', 'Tianjiupai.RoomServer.Callback':'make_initial_sync'()}}, {'ok', S834LastTable}}; {'wins_inning', S836WinnerSeat, S837LastTable, S838GainsQuad} -> {#{inning => {'inning_end', S838GainsQuad, 'Tianjiupai.RoomServer.Callback':'make_initial_sync'()}, meta => 'Tianjiupai.RoomServer.Callback':'deal_with_inning_end'(S831GameMeta, S836WinnerSeat, S838GainsQuad), snapshot_id => S830SnapshotId, sync => {'waiting_sync', 'Tianjiupai.RoomServer.Callback':'make_initial_sync'()}}, {'ok', S837LastTable}} end, begin S843CardOpts = case S829IsFront of true -> 'SesterlStdlib.List':'map'(fun(S841Card) -> {'ok', S841Card} end, S823Cards); false -> 'SesterlStdlib.List':'map'(fun(S842Card) -> 'error' end, S823Cards) end, begin S847Dummy = 'SesterlStdlib.List':'for_each'(fun(S844U) -> begin {'ok', S845Obs} = 'Tianjiupai.RoomServer.Callback':'make_observable_game_state'(S839GameState, S844U), begin S846Submission = #{new_state => S845Obs, seat => S826Seat, submitted => S843CardOpts, trick_last => S840LastTableOpt}, 'Tianjiupai.RoomServer.Callback':'notify'(S844U, [{'notify_submission', S846Submission} | []]) end end end, 'Tianjiupai.RoomServer.Callback':'the_other_players'(S839GameState, S826Seat)), begin {'ok', S848Obs} = 'Tianjiupai.RoomServer.Callback':'make_observable_game_state'(S839GameState, S822UserId), sesterl_internal_prim:'return'({'ok', {S848Obs, S840LastTableOpt, S839GameState}}) end end end end end end end end end end, case S849Opt of 'error' -> 'SesterlStdlib.GenServer':'reply'({'submission_done', 'error'}, S824State); {'ok', {S850Obs, S851LastTableOpt, S852GameState}} -> 'SesterlStdlib.GenServer':'reply'({'submission_done', {'ok', {S850Obs, S851LastTableOpt}}}, maps:put(room_state, {'internal_playing', S852GameState}, S824State)) end end.
'handle_ack'(S854UserId, S855SnapshotId, S856State) -> begin S865Opt = case maps:get(room_state, S856State) of {'internal_waiting', _} -> sesterl_internal_prim:'return'('error'); {'internal_playing', S857GameState} -> case 'Tianjiupai.RoomServer.Callback':'find_user_seat'(S857GameState, S854UserId) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S858Seat} -> case 'Tianjiupai.SnapshotId':'equal'(S855SnapshotId, maps:get(snapshot_id, S857GameState)) of true -> case maps:get(sync, S857GameState) of {'waiting_sync', S859SyncQuad} -> begin S860SyncQuad = 'Tianjiupai.Quad':'update'(S858Seat, true, S859SyncQuad), begin _ = sesterl_internal_prim:'print_debug'({<<"update sync">>, S855SnapshotId, maps:get(snapshot_id, S857GameState), S860SyncQuad}), case S860SyncQuad of {true, true, true, true} -> begin S861UserIds = 'Tianjiupai.RoomServer.Callback':'the_other_players'(S857GameState, S858Seat), begin S862Dummy = 'Tianjiupai.RoomServer.Callback':'notify_for_each'([S854UserId | S861UserIds], ['notify_next_step' | []]), begin S863GameState = maps:put(sync, 'waiting_input', S857GameState), sesterl_internal_prim:'return'({'ok', S863GameState}) end end end; _ -> begin S864GameState = maps:put(sync, {'waiting_sync', S860SyncQuad}, S857GameState), sesterl_internal_prim:'return'({'ok', S864GameState}) end end end end; 'waiting_input' -> begin _ = sesterl_internal_prim:'print_debug'({<<"waiting input">>, S855SnapshotId, maps:get(snapshot_id, S857GameState)}), sesterl_internal_prim:'return'('error') end end; false -> begin _ = sesterl_internal_prim:'print_debug'({<<"snapshot mismatch">>, S855SnapshotId, maps:get(snapshot_id, S857GameState)}), sesterl_internal_prim:'return'('error') end end end end, case S865Opt of 'error' -> 'SesterlStdlib.GenServer':'no_reply'(S856State); {'ok', S866GameState} -> 'SesterlStdlib.GenServer':'no_reply'(maps:put(room_state, {'internal_playing', S866GameState}, S856State)) end end.
'handle_require_next_inning'(S868UserId, S869SnapshotId, S870State) -> begin S881Opt = case maps:get(room_state, S870State) of {'internal_waiting', _} -> sesterl_internal_prim:'return'('error'); {'internal_playing', S871GameState} -> case 'Tianjiupai.RoomServer.Callback':'find_user_seat'(S871GameState, S868UserId) of 'error' -> sesterl_internal_prim:'return'('error'); {'ok', S872Seat} -> case 'Tianjiupai.SnapshotId':'equal'(S869SnapshotId, maps:get(snapshot_id, S871GameState)) of true -> case maps:get(inning, S871GameState) of {'during_inning', _} -> sesterl_internal_prim:'return'('error'); {'inning_end', S873GainsQuad, S874InningSyncQuad} -> begin S875InningSyncQuad = 'Tianjiupai.Quad':'update'(S872Seat, true, S874InningSyncQuad), begin _ = sesterl_internal_prim:'print_debug'({<<"update inning sync">>, S869SnapshotId, maps:get(snapshot_id, S871GameState), S875InningSyncQuad}), case S875InningSyncQuad of {true, true, true, true} -> begin S876GameState = 'Tianjiupai.RoomServer.Callback':'start_new_inning'(maps:get(meta, S871GameState)), begin S877UserIds = 'Tianjiupai.RoomServer.Callback':'the_other_players'(S876GameState, S872Seat), begin S880Dummy = 'SesterlStdlib.List':'for_each'(fun(S878U) -> begin {'ok', S879Obs} = 'Tianjiupai.RoomServer.Callback':'make_observable_game_state'(S876GameState, S878U), 'Tianjiupai.RoomServer.Callback':'notify'(S878U, [{'notify_game_start', S879Obs} | []]) end end, [S868UserId | S877UserIds]), sesterl_internal_prim:'return'({'ok', S876GameState}) end end end; _ -> sesterl_internal_prim:'return'({'ok', maps:put(inning, {'inning_end', S873GainsQuad, S875InningSyncQuad}, S871GameState)}) end end end end; false -> sesterl_internal_prim:'return'('error') end end end, case S881Opt of 'error' -> 'SesterlStdlib.GenServer':'no_reply'(S870State); {'ok', S882GameState} -> 'SesterlStdlib.GenServer':'no_reply'(maps:put(room_state, {'internal_playing', S882GameState}, S870State)) end end.
'update_sync'(S884UserId, S885State) -> case maps:get(room_state, S885State) of {'internal_waiting', _} -> sesterl_internal_prim:'return'(S885State); {'internal_playing', S886GameState} -> case 'Tianjiupai.RoomServer.Callback':'find_user_seat'(S886GameState, S884UserId) of 'error' -> sesterl_internal_prim:'return'(S885State); {'ok', S887Seat} -> case maps:get(sync, S886GameState) of 'waiting_input' -> sesterl_internal_prim:'return'(S885State); {'waiting_sync', S888SyncQuad} -> begin S889SyncQuad = 'Tianjiupai.Quad':'update'(S887Seat, true, S888SyncQuad), case S889SyncQuad of {true, true, true, true} -> begin S890UserIds = 'Tianjiupai.RoomServer.Callback':'the_other_players'(S886GameState, S887Seat), begin S891Dummy = 'Tianjiupai.RoomServer.Callback':'notify_for_each'(S890UserIds, ['notify_next_step' | []]), begin S892GameState = maps:put(sync, 'waiting_input', S886GameState), sesterl_internal_prim:'return'(maps:put(room_state, {'internal_playing', S892GameState}, S885State)) end end end; _ -> begin S893GameState = maps:put(sync, {'waiting_sync', S889SyncQuad}, S886GameState), sesterl_internal_prim:'return'(maps:put(room_state, {'internal_playing', S893GameState}, S885State)) end end end end end end.
'handle_call'(S895Req, S896From, S897State) -> case S895Req of 'get_whole_state' -> begin S898WholeState = 'Tianjiupai.RoomServer.Callback':'make_whole_room_state'(S897State), 'SesterlStdlib.GenServer':'reply'({'whole_state', S898WholeState}, S897State) end; {'get_personal_state', S899UserId} -> begin S900State = 'Tianjiupai.RoomServer.Callback':'update_sync'(S899UserId, S897State), begin S901PersonalStateOpt = 'Tianjiupai.RoomServer.Callback':'make_personal_room_state'(S900State, S899UserId), 'SesterlStdlib.GenServer':'reply'({'personal_state', S901PersonalStateOpt}, S900State) end end; {'send_chat', S902User, S903Text} -> 'Tianjiupai.RoomServer.Callback':'handle_send_chat'(S902User, S903Text, S897State); {'attend', S904User} -> 'Tianjiupai.RoomServer.Callback':'handle_attend'(S904User, S897State); {'exit', S905UserId} -> 'Tianjiupai.RoomServer.Callback':'handle_exit'(S905UserId, S897State); {'submit', S906UserId, S907Cards} -> 'Tianjiupai.RoomServer.Callback':'handle_submit'(S906UserId, S907Cards, S897State) end.
'handle_cast'(S909Msg, S910State) -> case S909Msg of {'ack', S911UserId, S912SnapshotId} -> 'Tianjiupai.RoomServer.Callback':'handle_ack'(S911UserId, S912SnapshotId, S910State); {'require_next_inning', S913UserId, S914SnapshotId} -> 'Tianjiupai.RoomServer.Callback':'handle_require_next_inning'(S913UserId, S914SnapshotId, S910State) end.
'handle_info'(S916Info, S917State) -> begin _ = sesterl_internal_prim:'print_debug'({<<"unexpected info">>, S916Info}), 'SesterlStdlib.GenServer':'no_reply'(S917State) end.
'terminate'(S919State) -> begin _ = sesterl_internal_prim:'print_debug'({<<"terminate">>, S919State}), sesterl_internal_prim:'return'(ok) end.