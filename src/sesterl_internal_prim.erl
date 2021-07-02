-module(sesterl_internal_prim).
-export([spawn/1, send/2, return/1, self/0, print_debug/1, format/2, float/1, round/1, truncate/1, decode_option/2, decode_option_with_default/3]).
spawn(F) -> erlang:spawn(F).
send(X, Y) -> X ! Y, ok.
return(X) -> X.
self() -> erlang:self().
print_debug(X) -> io:format("~p~n", [X]), ok.
format({Fmt, Arity}, Arg) -> Args = case Arg of ok -> []; _ -> tuple_to_list(Arg) end, lists:flatten(io_lib:format(Fmt, Args)).
float(N) -> N.
round(X) -> erlang:round(X).
truncate(X) -> erlang:trunc(X).
decode_option(Options, Key) -> maps:find(Key, Options).
decode_option_with_default(Options, Key, Thunk) -> case maps:find(Key, Options) of error -> Thunk(); {ok, Value} -> Value end.
