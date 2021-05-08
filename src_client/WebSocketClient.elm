module WebSocketClient exposing (setUserId, sendChat, subscribe)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Common exposing (..)
import Format exposing (..)
import Port


setUserId : UserId -> Cmd Msg
setUserId userId =
  let s = JE.encode 0 (setUserIdDataEncoder userId) in
  Port.sendWebSocketMessage s


sendChat : String -> Cmd Msg
sendChat text =
  let s = JE.encode 0 (sendChatDataEncoder text) in
  Port.sendWebSocketMessage s


decodeNotification : String -> Result JD.Error Notification
decodeNotification s =
  JD.decodeString notificationDecoder s


subscribe : Sub Msg
subscribe =
  Port.receiveWebSocketMessage (\s ->
    ReceiveNotification (decodeNotification s)
  )
