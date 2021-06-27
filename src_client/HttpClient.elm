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


getAllRooms : Origin -> Cmd Msg
getAllRooms origin =
  Http.get
    { url    = origin ++ "/rooms"
    , expect = Http.expectJson (ReceiveResponse << AllRoomsGot) decodeGetAllRoomsResponse
    }


getRoom : Origin -> UserId -> RoomId -> Cmd Msg
getRoom origin userId roomId =
  Http.get
    { url    = origin ++ "/rooms/" ++ roomId ++ "/users/" ++ userId
    , expect = Http.expectJson (ReceiveResponse << (RoomGot roomId)) decodeGetRoomResponse
    }


createUser : Origin -> UserName -> Cmd Msg
createUser origin userName =
  Http.post
    { url    = origin ++ "/users"
    , body   = Http.jsonBody (encodeCreateUserRequest { userName = userName })
    , expect = Http.expectJson (ReceiveResponse << (UserCreated userName)) decodeCreateUserResponse
    }


createRoom : Origin ->UserId -> RoomName -> Cmd Msg
createRoom origin userId roomName =
  Http.post
    { url    = origin ++ "/rooms"
    , body   = Http.jsonBody (encodeCreateRoomRequest { userId = userId, roomName = roomName })
    , expect = Http.expectJson (ReceiveResponse << (RoomCreated roomName)) decodeCreateRoomResponse
    }


enterRoom : Origin -> UserId -> RoomId -> Cmd Msg
enterRoom origin userId roomId =
  Http.request
    { method  = "PATCH"
    , headers = []
    , url     = origin ++ "/rooms/" ++ roomId
    , body    = Http.jsonBody (encodeRoomRequest (RoomRequestToEnterRoom { userId = userId }))
    , expect  = Http.expectJson (ReceiveResponse << (RoomEntered roomId)) decodeEnterRoomResponse
    , timeout = Nothing
    , tracker = Nothing
    }


submitCards : Origin -> UserId -> RoomId -> List Card -> Cmd Msg
submitCards origin userId roomId cards =
  Http.request
    { method  = "PATCH"
    , headers = []
    , url     = origin ++ "/rooms/" ++ roomId
    , body    = Http.jsonBody (encodeRoomRequest (RoomRequestToSubmitCards { userId = userId, cards = cards }))
    , expect  = Http.expectJson (ReceiveResponse << SubmissionDone) decodeSubmitCardsResponse
    , timeout = Nothing
    , tracker = Nothing
    }
