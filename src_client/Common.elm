module Common exposing (..)

import Url exposing (Url)
import Http
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation


type alias Flags = String
type alias UserId = String
type alias UserName = String
type alias RoomId = String
type alias RoomName = String

type alias User =
  { id        : UserId
  , name      : UserName
  , belongsTo : Maybe RoomId
  }

type alias Room =
  { id      : RoomId
  , name    : RoomName
  , members : List UserId
  }

type alias InputModel =
  { userName : String
  , roomName : String
  , chatText : String
  }

type alias Model =
  { navigationKey : Navigation.Key
  , message       : String
  , inputs        : InputModel
  , user          : Maybe User
  , rooms         : Maybe (List Room)
  }

type Request
  = CreateUser
  | CreateRoom
  | EnterRoom RoomId

type Response
  = UserCreated UserName (Result Http.Error UserId)
  | RoomCreated RoomName (Result Http.Error RoomId)
  | RoomEntered RoomId (Result Http.Error ())
  | RoomsGot (Result Http.Error (List Room))

type WebSocketRequest
  = SendChat

type InputUpdate
  = UserNameInput UserName
  | RoomNameInput RoomName
  | ChatInput String

type Msg
  = UrlChange Url
  | UrlRequest UrlRequest
  | UpdateInput InputUpdate
  | Send Request
  | Receive Response
  | SendWebSocketMessage WebSocketRequest
  | ReceiveWebSocketMessage String
