module HttpClient exposing
  ( createUser
  , createRoom
  , enterRoom
  , getRoom
  , getAllRooms
  , submitCards
  )

import Http

import Models exposing (..)
import Common exposing (..)


host : String
host =
  "http://localhost:8080"


createUser : UserName -> Cmd Msg
createUser userName =
  Http.post
    { url    = host ++ "/users"
    , body   = Http.jsonBody (encodeCreateUserRequest { userName = userName })
    , expect = Http.expectJson (ReceiveResponse << (UserCreated userName)) decodeCreateUserResponse
    }


createRoom : UserId -> RoomName -> Cmd Msg
createRoom userId roomName =
  Http.post
    { url    = host ++ "/rooms"
    , body   = Http.jsonBody (encodeCreateRoomRequest { userId = userId, roomName = roomName })
    , expect = Http.expectJson (ReceiveResponse << (RoomCreated roomName)) decodeCreateRoomResponse
    }


enterRoom : UserId -> RoomId -> Cmd Msg
enterRoom userId roomId =
  Http.request
    { method  = "PUT"
    , headers = []
    , url     = host ++ "/rooms/" ++ roomId
    , body    = Http.jsonBody (encodeEnterRoomRequest { userId = userId })
    , expect  = Http.expectJson (ReceiveResponse << (RoomEntered roomId)) decodeEnterRoomResponse
    , timeout = Nothing
    , tracker = Nothing
    }


getAllRooms : Cmd Msg
getAllRooms =
  Http.get
    { url    = host ++ "/rooms"
    , expect = Http.expectJson (ReceiveResponse << AllRoomsGot) decodeGetAllRoomsResponse
    }


getRoom : UserId -> RoomId -> Cmd Msg
getRoom userId roomId =
  Http.get
    { url    = host ++ "/rooms/" ++ roomId ++ "/users/" ++ userId
    , expect = Http.expectJson (ReceiveResponse << (RoomEntered roomId)) decodeGetRoomResponse
    }


submitCards : UserId -> RoomId -> List Card -> Cmd Msg
submitCards userId roomId cards =
  Http.request
    { method  = "PUT"
    , headers = []
    , url     = host ++ "/rooms/" ++ roomId ++ "/users/" ++ userId
    , body    = Http.jsonBody (encodeSubmitCardsRequest { cards = cards })
    , expect  = Http.expectJson (ReceiveResponse << SubmissionDone) decodeSubmitCardsResponse
    , timeout = Nothing
    , tracker = Nothing
    }
