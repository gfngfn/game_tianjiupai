module HttpClient exposing (createUser, createRoom, getRooms)

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


roomDecoder : Decoder Room
roomDecoder =
  JD.map3
    (\id name members ->
       { id      = id
       , name    = name
       , members = members
       }
    )
    (JD.field "room_id" JD.string)
    (JD.field "room_name" JD.string)
    (JD.field "members" (JD.list JD.string))


roomsDecoder : Decoder (List Room)
roomsDecoder =
  JD.field "rooms" (JD.list roomDecoder)


createRoomBodyEncoder : UserId -> RoomName -> JE.Value
createRoomBodyEncoder userId roomName =
  JE.object
    [ ( "user_id", JE.string userId )
    , ( "room_name", JE.string roomName )
    ]

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


getRooms : Cmd Response
getRooms =
  Http.get
    { url    = "http://" ++ host ++ "/rooms"
    , expect = Http.expectJson RoomsGot roomsDecoder
    }