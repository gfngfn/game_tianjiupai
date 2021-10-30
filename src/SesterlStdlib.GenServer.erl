-module('SesterlStdlib.GenServer').
-export(['init_ok'/1, 'init_ok'/2, 'init_stop'/1, 'reply'/2, 'reply'/3, 'reply_and_stop'/3, 'no_reply'/1, 'no_reply'/2, 'no_reply_and_stop'/2]).
'init_ok'(S164State) -> ?MODULE:'init_ok'(S164State, #{}).
'init_ok'(S164State, S2658) -> S165TimeoutOpt = sesterl_internal_prim:decode_option(S2658, timeout), sesterl_internal_prim:'return'({'ok', {S164State, S165TimeoutOpt}}).
'init_stop'(S167Reason) -> sesterl_internal_prim:'return'({'error', S167Reason}).
'reply'(S169Response, S170State) -> ?MODULE:'reply'(S169Response, S170State, #{}).
'reply'(S169Response, S170State, S2660) -> S171Timeout = sesterl_internal_prim:decode_option(S2660, timeout), sesterl_internal_prim:'return'({'reply_impl', S169Response, S170State, S171Timeout}).
'reply_and_stop'(S173Reason, S174Response, S175State) -> sesterl_internal_prim:'return'({'reply_and_stop_impl', S173Reason, S174Response, S175State}).
'no_reply'(S177State) -> ?MODULE:'no_reply'(S177State, #{}).
'no_reply'(S177State, S2662) -> S178Timeout = sesterl_internal_prim:decode_option(S2662, timeout), sesterl_internal_prim:'return'({'no_reply_impl', S177State, S178Timeout}).
'no_reply_and_stop'(S180Reason, S181State) -> sesterl_internal_prim:'return'({'no_reply_and_stop_impl', S180Reason, S181State}).
