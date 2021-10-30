-module(sesterl_internal_prim).
-export([spawn/1, send/2, return/1, self/0, print_debug/1, format/2, decode_option/2, decode_option_with_default/3]).
spawn(F) -> erlang:spawn(F).
send(Pid, Msg) -> Pid ! {'$sesterl', Msg}, ok.
return(X) -> X.
self() -> erlang:self().
print_debug(X) -> io:format("~p~n", [X]), ok.
format({Fmt, _Arity}, Arg) -> Args = case Arg of ok -> []; _ -> tuple_to_list(Arg) end, lists:flatten(io_lib:format(Fmt, Args)).
decode_option(Options, Key) -> maps:find(Key, Options).
decode_option_with_default(Options, Key, Thunk) -> case maps:find(Key, Options) of error -> Thunk(); {ok, Value} -> Value end.
