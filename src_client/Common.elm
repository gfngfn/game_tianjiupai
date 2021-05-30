module Common exposing (..)

import Url exposing (Url)
import Json.Decode as JD exposing (Decoder)
import Http

import Models exposing (..)
import Port exposing (..)


type alias Flags = String

type State
  = AtEntrance UserName (Maybe User)
    -- 1. The content of the input form for deciding usernames.
  | AtPlaza Port.WebSocket User RoomName (Maybe (List RoomSummary))
    -- 1. The user who is using the client
    -- 2. The content of the input form for creating new rooms
    -- 3. The list of existent rooms (which is temporarily `Nothing` if being fetched)
  | InRoom Port.WebSocket User PersonalState String
    -- 1. The user who is using the client
    -- 2. The state of the play where the user can observe
    -- 3. The content of the input form for chatting

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
  = UserCreated UserName (Result Http.Error CreateUserResponse)
  | RoomCreated RoomName (Result Http.Error CreateRoomResponse)
  | RoomEntered RoomId (Result Http.Error PersonalState)
  | AllRoomsGot (Result Http.Error GetAllRoomsResponse)

type InputUpdate
  = UserNameInput UserName
  | RoomNameInput RoomName
  | ChatInput String

type Msg
  = UpdateInput InputUpdate
  | SendRequest Request
  | ReceiveResponse Response
  | ReceiveNotification (Result JD.Error Notification)
  | OpenWebSocket Port.WebSocket
