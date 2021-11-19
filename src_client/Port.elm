port module Port exposing
  ( WebSocket
  , listenWebSocket
  , onOpenWebSocket
  , onCloseWebSocket
  , onErrorOfWebSocket
  , sendWebSocketMessage
  , receiveWebSocketMessage
  )

import Json.Encode


type alias WebSocket = Json.Encode.Value

port listenWebSocket : String -> Cmd msg

port onOpenWebSocket : (WebSocket -> msg) -> Sub msg

port onCloseWebSocket : (WebSocket -> msg) -> Sub msg

port onErrorOfWebSocket : (String -> msg) -> Sub msg

port sendWebSocketMessage : ( WebSocket, String ) -> Cmd msg

port receiveWebSocketMessage : (String -> msg) -> Sub msg
