module HttpClient exposing (createUser, createRoom, enterRoom, getRoom, getAllRooms)

import Http

import Common exposing (..)
import Format exposing (..)


host : String
host =
  "http://localhost:8080"


createUser : UserName -> Cmd Msg
createUser userName =
  Http.post
    { url    = host ++ "/users"
    , body   = Http.jsonBody (createUserBodyEncoder userName)
    , expect = Http.expectJson (ReceiveResponse << (UserCreated userName)) userIdDecoder
    }


createRoom : UserId -> RoomName -> Cmd Msg
createRoom userId roomName =
  Http.post
    { url    = host ++ "/rooms"
    , body   = Http.jsonBody (createRoomBodyEncoder userId roomName)
    , expect = Http.expectJson (ReceiveResponse << (RoomCreated roomName)) roomIdDecoder
    }


enterRoom : UserId -> RoomId -> Cmd Msg
enterRoom userId roomId =
  Http.request
    { method  = "PUT"
    , headers = []
    , url     = host ++ "/rooms/" ++ roomId
    , body    = Http.jsonBody (enterRoomBodyEncoder userId)
    , expect  = Http.expectJson (ReceiveResponse << (RoomEntered roomId)) roomDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


getAllRooms : Cmd Msg
getAllRooms =
  Http.get
    { url    = host ++ "/rooms"
    , expect = Http.expectJson (ReceiveResponse << AllRoomsGot) roomsDecoder
    }


getRoom : RoomId -> Cmd Msg
getRoom roomId =
  Http.get
    { url    = host ++ "/rooms/" ++ roomId
    , expect = Http.expectJson (ReceiveResponse << (RoomEntered roomId)) roomDecoder
    }
