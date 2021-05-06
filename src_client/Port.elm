port module Port exposing (..)


port sendWebSocketMessage : String -> Cmd msg

port receiveWebSocketMessage : (String -> msg) -> Sub msg
