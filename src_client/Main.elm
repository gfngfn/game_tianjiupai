module Main exposing (..)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)
import Url exposing (Url)
import Http
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation

import Common exposing (..)
import Flag
import HttpClient
import WebSocketClient
import View


main =
  Browser.application
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , onUrlChange   = UrlChange
    , onUrlRequest  = UrlRequest
    , view          = view
    }


init : String -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flagString url navKey =
  let
    maybeUser : Maybe User
    maybeUser =
      case Flag.decode flagString of
          Ok(maybeUser0) -> maybeUser0
          Err(_)         -> Nothing

    ( cmd, state ) =
      case maybeUser of
        Nothing ->
          ( Cmd.none, AtEntrance "" )

        Just user ->
          let cmd1 = WebSocketClient.setUserId user.id in
          case user.belongsTo of
            Nothing ->
              let cmd2 = HttpClient.getAllRooms in
              ( Cmd.batch [ cmd1, cmd2 ], AtEntrance "" )

            Just roomId ->
              let cmd2 = HttpClient.getRoom roomId in
              ( Cmd.batch [ cmd1, cmd2 ], AtPlaza user "" Nothing )

    model : Model
    model =
      { navigationKey = navKey
      , message       = "flags: " ++ flagString
      , state         = state
      }
  in
  ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.state of
    AtEntrance userNameInput ->
      case msg of
        UpdateInput (UserNameInput userNameInput1) ->
          ( { model | state = AtEntrance userNameInput1 }, Cmd.none )

        SendRequest CreateUser ->
          let cmd = HttpClient.createUser userNameInput in
          ( model, cmd )

        ReceiveResponse (UserCreated userName result) ->
          case result of
            Ok userId ->
              let user = { id = userId, name = userName, belongsTo = Nothing } in
              let cmd1 = WebSocketClient.setUserId userId in
              let cmd2 = HttpClient.getAllRooms in
              ( { model | state = AtPlaza user "" Nothing }, Cmd.batch [ cmd1, cmd2 ] )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (AtEntrance): " ++ showMessage msg }, Cmd.none )

    AtPlaza user roomNameInput0 maybeRooms ->
      case msg of
        UpdateInput (RoomNameInput roomNameInput1) ->
           ( { model | state = AtPlaza user roomNameInput1 maybeRooms }, Cmd.none )

        ReceiveResponse (AllRoomsGot result) ->
          case result of
            Ok rooms ->
              ( { model | state = AtPlaza user roomNameInput0 (Just rooms) }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        SendRequest CreateRoom ->
          let cmd = HttpClient.createRoom user.id roomNameInput0 in
          ( { model | state = AtPlaza user "" maybeRooms }, cmd )

        ReceiveResponse (RoomCreated roomName result) ->
          case result of
            Ok roomId ->
              case maybeRooms of
                Just rooms0 ->
                  let room = { id = roomId, name = roomName, members = [], logs = [] } in
                  ( { model | state = AtPlaza user roomNameInput0 (Just (rooms0 ++ [room])) }, Cmd.none )

                Nothing ->
                  ( model, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        SendRequest (EnterRoom roomId) ->
          let cmd = HttpClient.enterRoom user.id roomId in
          ( model, cmd )

        ReceiveResponse (RoomEntered roomId result) ->
          case result of
            Ok room ->
              ( { model | state = InRoom user room "" WaitingStart }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (AtPlaza): " ++ showMessage msg }, Cmd.none )

    InRoom user room0 chatTextInput0 roomState ->
      case ( roomState, msg ) of
        ( _, UpdateInput (ChatInput chatTextInput1) ) ->
           ( { model | state = InRoom user room0 chatTextInput1 roomState }, Cmd.none )

        ( _, SendRequest SendChat ) ->
          let cmd = WebSocketClient.sendChat chatTextInput0 in
          ( { model | state = InRoom user room0 "" roomState  }, cmd )

        ( _, ReceiveNotification (Err err) ) ->
          ( { model | message = "[warning] invalid notification" }, Cmd.none )

        ( _, ReceiveNotification (Ok (LogNotification log)) ) ->
          let room1 = { room0 | logs = room0.logs ++ [log] } in
          ( { model | state = InRoom user room1 chatTextInput0 roomState }, Cmd.none )

        ( WaitingStart, ReceiveNotification (Ok GameStarted) ) ->
          ( { model | state = InRoom user room0 chatTextInput0 PlayingGame }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (InRoom): " ++ showMessage msg }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
  { title = "tianjiupai"
  , body  = View.viewBody model
  }


showNotification : Notification -> String
showNotification nt =
  case nt of
    LogNotification _ -> "LogNotification"
    GameStarted       -> "GameStarted"


showMessage : Msg -> String
showMessage msg =
  case msg of
    UrlChange _                 -> "UrlChange"
    UrlRequest _                -> "UrlRequest"
    UpdateInput _               -> "UpdateInput"
    SendRequest _               -> "SendRequest"
    ReceiveResponse _           -> "ReceiveResponse"
    ReceiveNotification (Err _) -> "ReceiveNotification (error)"
    ReceiveNotification (Ok nt) -> "ReceiveNotification (" ++ showNotification nt ++ ")"


makeErrorMessage : Http.Error -> String
makeErrorMessage err =
  let
    msg =
      case err of
        Http.BadUrl s     -> "bad URL; " ++ s
        Http.Timeout      -> "request timeout"
        Http.BadBody s    -> "bad body; " ++ s
        Http.NetworkError -> "network error"
        Http.BadStatus n  -> "bad status; " ++ String.fromInt n
  in
  "[error] " ++ msg


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocketClient.subscribe
