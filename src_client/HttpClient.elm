module HttpClient exposing (createUser, createRoom, enterRoom, getRoom, getAllRooms)

import Http

import Common exposing (..)
import Format exposing (..)


host : String
host =
  "localhost:8080"


createUser : UserName -> Cmd Response
createUser userName =
  Http.post
    { url    = "http://" ++ host ++ "/users"
    , body   = Http.jsonBody (createUserBodyEncoder userName)
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
