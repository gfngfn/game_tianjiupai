module WebSocketClient exposing (setUserId, sendChat, subscribe)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Models exposing (..)
import Common exposing (..)
import Port


setUserId : UserId -> Cmd Msg
setUserId userId =
  let s = JE.encode 0 (encodeCommand (CommandSetUserId userId)) in
  Port.sendWebSocketMessage s


sendChat : String -> Cmd Msg
sendChat text =
  let s = JE.encode 0 (encodeCommand (CommandComment text)) in
  Port.sendWebSocketMessage s


subscribe : Sub Msg
subscribe =
  Port.receiveWebSocketMessage (\s ->
    ReceiveNotification (JD.decodeString decodeNotification s)
  )
