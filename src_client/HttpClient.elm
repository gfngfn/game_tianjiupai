module HttpClient exposing (createUser, createRoom, enterRoom, getRoom, getAllRooms)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Http

import Common exposing (..)


host : String
host =
  "localhost:8080"


userIdDecoder : Decoder UserId
userIdDecoder =
  JD.field "user_id" JD.string


roomIdDecoder : Decoder RoomId
roomIdDecoder =
  JD.field "room_id" JD.string


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


createUser : UserName -> Cmd Response
createUser userName =
  Http.post
    { url    = "http://" ++ host ++ "/users"
    , body   = Http.jsonBody (JE.object [ ( "user_name", JE.string userName ) ])
    , expect = Http.expectJson (UserCreated userName) userIdDecoder
    }


createRoom : UserId -> RoomName -> Cmd Response
createRoom userId roomName =
  Http.post
    { url    = "http://" ++ host ++ "/rooms"
    , body   = Http.jsonBody (createRoomBodyEncoder userId roomName)
    , expect = Http.expectJson (RoomCreated roomName) roomIdDecoder
    }


enterRoom : UserId -> RoomId -> Cmd Response
enterRoom userId roomId =
  Http.request
    { method  = "PUT"
    , headers = []
    , url     = "http://" ++ host ++ "/rooms/" ++ roomId
    , body    = Http.jsonBody (enterRoomBodyEncoder userId)
    , expect  = Http.expectJson (RoomEntered roomId) roomDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


getAllRooms : Cmd Response
getAllRooms =
  Http.get
    { url    = "http://" ++ host ++ "/rooms"
    , expect = Http.expectJson AllRoomsGot roomsDecoder
    }


getRoom : RoomId -> Cmd Response
getRoom roomId =
  Http.get
    { url    = "http://" ++ host ++ "/rooms/" ++ roomId
    , expect = Http.expectJson (RoomEntered roomId) roomDecoder
    }
