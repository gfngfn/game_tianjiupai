module Format exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE

import Common exposing (..)


flagUserDecoder : Decoder User
flagUserDecoder =
    JD.map3
      (\userId userName belongsTo -> { id = userId, name = userName, belongsTo = belongsTo })
      (JD.field "id" JD.string)
      (JD.field "name" JD.string)
      (JD.field "belongs_to" (maybeDecoder JD.string))



flagDecoder : Decoder (Maybe User)
flagDecoder =
  JD.field "user" (maybeDecoder flagUserDecoder)


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


userIdDecoder : Decoder UserId
userIdDecoder =
  JD.field "user_id" JD.string


roomIdDecoder : Decoder RoomId
roomIdDecoder =
  JD.field "room_id" JD.string


roomDecoder : Decoder Room
roomDecoder =
  JD.map4
    (\id name members logs ->
       { id      = id
       , name    = name
       , members = members
       , logs    = logs
       }
    )
    (JD.field "room_id" JD.string)
    (JD.field "room_name" JD.string)
    (JD.field "members" (JD.list JD.string))
    (JD.field "logs" (JD.list logDecoder))


roomsDecoder : Decoder (List Room)
roomsDecoder =
  JD.field "rooms" (JD.list roomDecoder)


maybeDecoder : Decoder a -> Decoder (Maybe a)
maybeDecoder jd =
  JD.field "type" JD.string
    |> JD.andThen (\s ->
      case s of
        "nothing" ->
          JD.succeed Nothing

        "just" ->
          JD.field "value" jd |> JD.map Just

        _ ->
          JD.fail "other than 'just' or 'nothing'"
    )


createUserBodyEncoder : UserName -> JE.Value
createUserBodyEncoder userName =
  JE.object [ ( "user_name", JE.string userName ) ]

createRoomBodyEncoder : UserId -> RoomName -> JE.Value
createRoomBodyEncoder userId roomName =
  JE.object
    [ ( "user_id", JE.string userId )
    , ( "room_name", JE.string roomName )
    ]


enterRoomBodyEncoder : UserId -> JE.Value
enterRoomBodyEncoder userId =
  JE.object
    [ ( "user_id", JE.string userId ) ]


logDecoder : Decoder Log
logDecoder =
  JD.field "type" JD.string
    |> JD.andThen (\s ->
      case s of
        "comment" ->
          JD.map2
            (\from text -> LogComment from text)
            (JD.field "from" JD.string)
            (JD.field "text" JD.string)

        "entered" ->
          JD.map LogEntered (JD.field "user_id" JD.string)

        "exited" ->
          JD.map LogEntered (JD.field "user_id" JD.string)

        _ ->
          JD.fail "other than 'comment', 'entered', or 'exited'"
    )


notificationDecoder : Decoder WebSocketNotification
notificationDecoder =
  JD.field "command" JD.string
    |> JD.andThen (\cmd ->
      case cmd of
        "log" ->
          JD.map LogNotification (JD.field "value" logDecoder)

        _ ->
          JD.fail ("unknown notifiaction command '" ++ cmd ++ "'")
    )
