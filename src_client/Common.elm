module Common exposing (..)

import Url exposing (Url)
import Json.Decode as JD exposing (Decoder)
import Http


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

type Log
  = LogComment UserId String
  | LogEntered UserId
  | LogExited UserId

type alias Room =
  { id      : RoomId
  , name    : RoomName
  , members : List UserId
  , logs    : List Log
  }

type alias InputModel =
  { userName : String
  , roomName : String
  , chatText : String
  }

type RoomState
  = WaitingStart
  | PlayingGame

type State
  = AtEntrance UserName
  | AtPlaza User String (Maybe (List Room))
  | InRoom User Room String RoomState

type alias Model =
  { message       : String
  , state         : State
  }

type Request
  = CreateUser
  | CreateRoom
  | EnterRoom RoomId
  | SendChat

type Response
  = UserCreated UserName (Result Http.Error UserId)
  | RoomCreated RoomName (Result Http.Error RoomId)
  | RoomEntered RoomId (Result Http.Error Room)
  | AllRoomsGot (Result Http.Error (List Room))

type Notification
  = LogNotification Log
  | GameStarted

type InputUpdate
  = UserNameInput UserName
  | RoomNameInput RoomName
  | ChatInput String

type Msg
  = UpdateInput InputUpdate
  | SendRequest Request
  | ReceiveResponse Response
  | ReceiveNotification (Result JD.Error Notification)
