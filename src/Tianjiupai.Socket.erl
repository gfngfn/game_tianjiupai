-module('Tianjiupai.Socket').
-export(['connect'/2, 'send'/2]).
    connect(Address, Port) ->
        gen_tcp:connect(Address, Port, []).
  
    send(Socket, Packet) ->
        Result = gen_tcp:send(Socket, Packet),
        case Result of
            ok         -> {ok, ok};
            {error, _} -> Result
        end.
  
