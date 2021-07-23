-module('SesterlStdlib.GenServer').
-export(['init_ok'/1, 'init_stop'/1, 'reply'/2, 'reply_fast'/2, 'reply_and_stop'/3, 'no_reply'/1, 'stop'/2]).
'init_ok'(S91State) -> sesterl_internal_prim:'return'({'ok', S91State}).
'init_stop'(S93Reason) -> sesterl_internal_prim:'return'({'error', S93Reason}).
'reply'(S95Response, S96State) -> sesterl_internal_prim:'return'({'reply_impl', S95Response, fun() -> sesterl_internal_prim:'return'(S96State) end}).
'reply_fast'(S98Response, S99Comp) -> sesterl_internal_prim:'return'({'reply_impl', S98Response, S99Comp}).
'reply_and_stop'(S101Reason, S102Response, S103State) -> sesterl_internal_prim:'return'({'reply_and_stop', S101Reason, S102Response, S103State}).
'no_reply'(S105State) -> sesterl_internal_prim:'return'({'no_reply_impl', S105State}).
'stop'(S107Reason, S108State) -> sesterl_internal_prim:'return'({'stop', S107Reason, S108State}).
