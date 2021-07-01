-module('SesterlStdlib.GenServer').
-export(['init_ok'/1, 'init_stop'/1, 'reply'/2, 'reply_fast'/2, 'no_reply'/1]).
'init_ok'(S87State) -> sesterl_internal_prim:'return'({'ok', S87State}).
'init_stop'(S89Reason) -> sesterl_internal_prim:'return'({'error', S89Reason}).
'reply'(S91Response, S92State) -> sesterl_internal_prim:'return'({'reply_impl', S91Response, fun() -> sesterl_internal_prim:'return'(S92State) end}).
'reply_fast'(S94Response, S95Comp) -> sesterl_internal_prim:'return'({'reply_impl', S94Response, S95Comp}).
'no_reply'(S97State) -> sesterl_internal_prim:'return'({'no_reply_impl', S97State}).
