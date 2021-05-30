module WebSocketClient exposing
  ( WebSocket
  , listen
  , onOpen
  , setUserId
  , sendChat
  , subscribe
  )

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Models exposing (..)
import Common exposing (..)
import Port


type alias WebSocket = Port.WebSocket


listen : String -> Cmd Msg
listen url =
  Port.listenWebSocket url


onOpen : Sub Msg
onOpen =
  Port.onOpenWebSocket (\ws ->
    OpenWebSocket ws
  )


setUserId : WebSocket -> UserId -> Cmd Msg
setUserId ws userId =
  let s = JE.encode 0 (encodeCommand (CommandSetUserId userId)) in
  Port.sendWebSocketMessage ( ws, s )


sendChat : WebSocket -> String -> Cmd Msg
sendChat ws text =
  let s = JE.encode 0 (encodeCommand (CommandComment text)) in
  Port.sendWebSocketMessage ( ws, s )


subscribe : Sub Msg
subscribe =
  Port.receiveWebSocketMessage (\s ->
    ReceiveNotification (JD.decodeString decodeNotification s)
  )
