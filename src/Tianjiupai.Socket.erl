-module('Tianjiupai.Socket').
-export(['connect'/2, 'send'/2]).
    connect(Address, Port) ->
        gen_tcp:connect(Address, Port, []).
  
    send(Socket, Packet) ->
        gen_tcp:send(Socket, Packet).
  
