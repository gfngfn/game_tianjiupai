module Common exposing (..)

import Url exposing (Url)
import Http
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation


type alias Flags = String

type alias UserId = String

type alias UserName = String

type alias InputModel =
  { userName : String
  , chatText : String
  }

type alias Model =
  { navigationKey : Navigation.Key
  , message       : String
  , inputs        : InputModel
  , user          : Maybe User
  }

type Request
  = CreateUser

type Response
  = UserCreated UserName (Result Http.Error UserId)

type WebSocketRequest
  = SendChat

type InputUpdate
  = UserNameInput UserName
  | ChatInput String

type Msg
  = UrlChange Url
  | UrlRequest UrlRequest
  | UpdateInput InputUpdate
  | Send Request
  | Receive Response
  | SendWebSocketMessage WebSocketRequest
  | ReceiveWebSocketMessage String

type alias User =
  { id   : UserId
  , name : UserName
  }
