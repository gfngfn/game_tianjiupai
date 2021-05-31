module Main exposing (..)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)
import Url exposing (Url)
import Http
import Browser

import Common exposing (..)
import Models exposing (..)
import HttpClient
import WebSocketClient
import View


main =
  Browser.document
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }


init : String -> ( Model, Cmd Msg )
init flagString =
  let
    maybeFlagUser : Maybe FlagUser
    maybeFlagUser =
      case JD.decodeString decodeFlag flagString of
          Ok(flag) -> flag.user
          Err(_)   -> Nothing

    ( cmd, state ) =
      case maybeFlagUser of
        Nothing ->
          ( Cmd.none, AtEntrance "" Nothing )

        Just flagUser ->
          let userId = flagUser.id in
          let userName = flagUser.name in
          let cmd0 = WebSocketClient.listen userId in
          let
            user : User
            user =
              { userId = userId, userName = userName }
          in
          ( cmd0, AtEntrance "" (Just ( user, flagUser.belongsTo )) )

    model : Model
    model =
      { message = "flags: " ++ flagString
      , state   = state
      }
  in
  ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.state of
    AtEntrance userNameInput maybeUserAndRoom ->
      case msg of
        UpdateInput (UserNameInput userNameInput1) ->
          ( { model | state = AtEntrance userNameInput1 maybeUserAndRoom }, Cmd.none )

        SendRequest CreateUser ->
          let cmd = HttpClient.createUser userNameInput in
          ( model, cmd )

        ReceiveResponse (UserCreated userName result) ->
          case result of
            Ok responseBody ->
              let userId = responseBody.userId in
              let
                user : User
                user = { userId = userId, userName = userName }
              in
              let cmd = WebSocketClient.listen userId in
              ( { model | state = AtEntrance userNameInput (Just ( user, Nothing )) }, cmd )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        OpenWebSocket ws ->
            case maybeUserAndRoom of
              Just ( user, Nothing ) ->
                let cmd = HttpClient.getAllRooms in
                ( { model | state = AtPlaza ws user "" Nothing }, cmd )

              Just ( user, Just roomId ) ->
                let cmd = HttpClient.getRoom user.userId roomId in
                ( { model | state = AtPlaza ws user "" Nothing }, cmd )

              Nothing ->
                ( { model | message = "[warning] unexpected message (AtEntrance): " ++ showMessage msg }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (AtEntrance): " ++ showMessage msg }, Cmd.none )

    AtPlaza ws user roomNameInput0 maybeRooms ->
      case msg of
        UpdateInput (RoomNameInput roomNameInput1) ->
           ( { model | state = AtPlaza ws user roomNameInput1 maybeRooms }, Cmd.none )

        ReceiveResponse (AllRoomsGot result) ->
          case result of
            Ok responseBody ->
              let rooms = responseBody.rooms in
              ( { model | state = AtPlaza ws user roomNameInput0 (Just rooms) }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        SendRequest CreateRoom ->
          let cmd = HttpClient.createRoom user.userId roomNameInput0 in
          ( { model | state = AtPlaza ws user "" maybeRooms }, cmd )

        ReceiveResponse (RoomCreated roomName result) ->
          case result of
            Ok responseBody ->
              case maybeRooms of
                Just roomSummaries0 ->
                  let
                    roomId : RoomId
                    roomId =
                      responseBody.roomId

                    roomSummary : RoomSummary
                    roomSummary =
                      { room      = { roomId = roomId, roomName = roomName }
                      , members   = []
                      , isPlaying = False
                      }

                    roomSummaries1 : List RoomSummary
                    roomSummaries1 =
                      roomSummaries0 ++ [roomSummary]
                  in
                  ( { model | state = AtPlaza ws user roomNameInput0 (Just roomSummaries1) }, Cmd.none )

                Nothing ->
                  ( model, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        SendRequest (EnterRoom roomId) ->
          let cmd = HttpClient.enterRoom user.userId roomId in
          ( model, cmd )

        ReceiveResponse (RoomEntered roomId result) ->
          case result of
            Ok room ->
              ( { model | state = InRoom ws user room "" }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (AtPlaza): " ++ showMessage msg }, Cmd.none )

    InRoom ws user pstate0 chatTextInput0 ->
      case ( pstate0.game, msg ) of
        ( _, UpdateInput (ChatInput chatTextInput1) ) ->
           ( { model | state = InRoom ws user pstate0 chatTextInput1 }, Cmd.none )

        ( _, SendRequest SendChat ) ->
          let cmd = WebSocketClient.sendChat ws chatTextInput0 in
          ( { model | state = InRoom ws user pstate0 ""  }, cmd )

        ( _, ReceiveNotification (Err err) ) ->
          ( { model | message = "[warning] invalid notification" }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyComment comment)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogComment comment ] } in
          ( { model | state = InRoom ws user pstate1 chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyEntered userIdEntered)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogEntered userIdEntered ] } in
          ( { model | state = InRoom ws user pstate1 chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyExited userIdExited)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogExited userIdExited ] } in
          ( { model | state = InRoom ws user pstate1 chatTextInput0 }, Cmd.none )

        ( WaitingStart _, ReceiveNotification (Ok (NotifyGameStart ostate)) ) ->
          let cmd = Debug.todo "TODO: send sync with snapshot ID" in
          ( { model | state = InRoom ws user { pstate0 | game = PlayingGame ostate } chatTextInput0 }, cmd )

        ( PlayingGame ostate0, ReceiveNotification (Ok (NotifySubmission submission)) ) ->
          if ostate0.synchronizing then
            ( { model | message = "[warning] unexpected message (InRoom): " ++ showMessage msg }, Cmd.none )
          else
            let cmd = Debug.todo "TODO: send sync with snapshot ID" in
            let ostate1 = ostate0 |> updateObservableGameStateBySubmission submission in
            ( { model | state = InRoom ws user { pstate0 | game = PlayingGame ostate1 } chatTextInput0 }, cmd )

        ( PlayingGame ostate0, ReceiveNotification (Ok NotifyNextStep) ) ->
          if ostate0.synchronizing then
            let ostate1 = { ostate0 | synchronizing = False } in
            ( { model | state = InRoom ws user { pstate0 | game = PlayingGame ostate1 } chatTextInput0 }, Cmd.none )
          else
            ( { model | message = "[warning] unexpected message (InRoom): " ++ showMessage msg }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (InRoom): " ++ showMessage msg }, Cmd.none )


updateObservableGameStateBySubmission : Submission -> ObservableGameState -> ObservableGameState
updateObservableGameStateBySubmission submission ostate0 =
  Debug.todo "TODO: updateObservableGameStateBySubimission; don't forget to set 'synchronizing' to 'true'!"


view : Model -> Browser.Document Msg
view model =
  { title = "tianjiupai"
  , body  = View.viewBody model
  }


showNotification : Notification -> String
showNotification notification =
  case notification of
    NotifyComment _    -> "NotifyComment"
    NotifyEntered _    -> "NotifyEntered"
    NotifyExited _     -> "NotifyExited"
    NotifyGameStart _  -> "NotifyGameStart"
    NotifyNextStep     -> "NotifyNextStep"
    NotifySubmission _ -> "NotifySubmission"


showMessage : Msg -> String
showMessage msg =
  case msg of
    UpdateInput _               -> "UpdateInput"
    SendRequest _               -> "SendRequest"
    ReceiveResponse _           -> "ReceiveResponse"
    ReceiveNotification (Err _) -> "ReceiveNotification (error)"
    ReceiveNotification (Ok nt) -> "ReceiveNotification (" ++ showNotification nt ++ ")"
    OpenWebSocket _             -> "OpenWebSocket"


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
    Sub.batch
      [ WebSocketClient.onOpen
      , WebSocketClient.subscribe
      ]
