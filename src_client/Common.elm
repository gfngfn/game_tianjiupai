module Common exposing (..)

import Set exposing (Set)
import Url exposing (Url)
import Json.Decode as JD exposing (Decoder)
import Process
import Task
import Http

import Models exposing (..)
import Port exposing (..)


type alias Flags = String

type State
  = AtEntrance UserName (Maybe ( User, Maybe RoomId ))
    -- 1. The content of the input form for deciding usernames.
  | AtPlaza Port.WebSocket User RoomName (Maybe (List RoomSummary))
    -- 1. The user who is using the client
    -- 2. The content of the input form for creating new rooms
    -- 3. The list of existent rooms (which is temporarily `Nothing` if being fetched)
  | InRoom Port.WebSocket User PersonalState (Set Int) String
    -- 1. The user who is using the client
    -- 2. The state of the play where the user can observe
    -- 3. The indices of cards the user is selecting
    -- 4. The content of the input form for chatting

type MessageLevel
  = Warning
  | Information

type alias Origin = String

type alias Model =
  { message : ( MessageLevel, String )
  , state   : State
  , window  : { width : Int, height : Int }
  , origin  : Origin
  }

type Request
  = CreateUser
  | DeleteUser
  | CreateRoom
  | EnterRoom RoomId
  | ExitRoom RoomId
  | SendChat
  | SubmitCards
  | RequireNextInning

type Response
  = UserCreated UserName (Result Http.Error CreateUserResponse)
  | UserDeleted UserId (Result Http.Error ())
  | RoomCreated RoomName (Result Http.Error CreateRoomResponse)
  | RoomGot RoomId (Result Http.Error PersonalState)
  | RoomEntered RoomId (Result Http.Error EnterRoomResponse)
  | RoomExited RoomId (Result Http.Error ExitRoomResponse)
  | AllRoomsGot (Result Http.Error GetAllRoomsResponse)
  | SubmissionDone (Result Http.Error SubmitCardsResponse)

type InputUpdate
  = UserNameInput UserName
  | RoomNameInput RoomName
  | ChatInput String

type Msg
  = UpdateInput InputUpdate
  | SendRequest Request
  | SelectCard Int
  | UnselectCard Int
  | ReceiveResponse Response
  | ReceiveNotification (Result JD.Error Notification)
  | OpenWebSocket Port.WebSocket
  | TransitionToNextTrick ObservableGameState
  | Heartbeat
  | WindowResized Int Int


getSelectedCards : Set Int -> List Card -> List Card
getSelectedCards indices cards =
  cards
    |> List.indexedMap (\index card -> ( index, card ))
    |> List.filterMap (\( index, card ) -> if indices |> Set.member index then Just card else Nothing)


sendAfter : Float -> msg -> Cmd msg
sendAfter time msg =
  Process.sleep time
    |> Task.andThen (\() -> Task.succeed msg)
    |> Task.perform (\msg0 -> msg0)
