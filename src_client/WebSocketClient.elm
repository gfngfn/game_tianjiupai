module WebSocketClient exposing
  ( WebSocket
  , listen
  , onOpen
  , sendChat
  , subscribe
  )

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Models exposing (..)
import Common exposing (..)
import Port


type alias WebSocket = Port.WebSocket


host : String
host =
  "ws://localhost:8080"


listen : UserId -> Cmd Msg
listen userId =
  Port.listenWebSocket (host ++ "/websocket/" ++ userId)


onOpen : Sub Msg
onOpen =
  Port.onOpenWebSocket (\ws ->
    OpenWebSocket ws
  )


sendChat : WebSocket -> String -> Cmd Msg
sendChat ws text =
  let s = JE.encode 0 (encodeCommand (CommandComment text)) in
  Port.sendWebSocketMessage ( ws, s )


subscribe : Sub Msg
subscribe =
  Port.receiveWebSocketMessage (\s ->
    ReceiveNotification (JD.decodeString decodeNotification s)
  )
