module WebSocketClient exposing (setUserId, sendChat, subscribe)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Common exposing (..)
import Port


setUserIdDataEncoder : UserId -> JE.Value
setUserIdDataEncoder userId =
  JE.object
    [ ( "command", JE.string "set_user_id" )
    , ( "user_id", JE.string userId )
    ]


sendChatDataEncoder text =
  JE.object
    [ ( "command", JE.string "send_chat" )
    , ( "text",    JE.string text )
    ]


setUserId : UserId -> Cmd Msg
setUserId userId =
  let s = JE.encode 0 (setUserIdDataEncoder userId) in
  Port.sendWebSocketMessage s


sendChat : String -> Cmd Msg
sendChat text =
  let s = JE.encode 0 (sendChatDataEncoder text) in
  Port.sendWebSocketMessage s


subscribe : Sub Msg
subscribe =
  Port.receiveWebSocketMessage ReceiveWebSocketMessage
