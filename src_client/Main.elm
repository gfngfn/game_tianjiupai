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
          ( Cmd.none, AtEntrance "" )

        Just flagUser ->
          let userId = flagUser.id in
          let userName = flagUser.name in
          let cmd1 = WebSocketClient.setUserId userId in
          let
            user : User
            user =
              { userId = userId, userName = userName }
          in
          case flagUser.belongsTo of
            Nothing ->
              let cmd2 = HttpClient.getAllRooms in
              ( Cmd.batch [ cmd1, cmd2 ], AtPlaza user "" Nothing )

            Just roomId ->
              let cmd2 = HttpClient.getRoom userId roomId in
              ( Cmd.batch [ cmd1, cmd2 ], AtPlaza user "" Nothing )

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
    AtEntrance userNameInput ->
      case msg of
        UpdateInput (UserNameInput userNameInput1) ->
          ( { model | state = AtEntrance userNameInput1 }, Cmd.none )

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
            Ok responseBody ->
              let rooms = responseBody.rooms in
              ( { model | state = AtPlaza user roomNameInput0 (Just rooms) }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        SendRequest CreateRoom ->
          let cmd = HttpClient.createRoom user.userId roomNameInput0 in
          ( { model | state = AtPlaza user "" maybeRooms }, cmd )

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
                  ( { model | state = AtPlaza user roomNameInput0 (Just roomSummaries1) }, Cmd.none )

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
              ( { model | state = InRoom user room "" }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (AtPlaza): " ++ showMessage msg }, Cmd.none )

    InRoom user pstate0 chatTextInput0 ->
      case ( pstate0.game, msg ) of
        ( _, UpdateInput (ChatInput chatTextInput1) ) ->
           ( { model | state = InRoom user pstate0 chatTextInput1 }, Cmd.none )

        ( _, SendRequest SendChat ) ->
          let cmd = WebSocketClient.sendChat chatTextInput0 in
          ( { model | state = InRoom user pstate0 ""  }, cmd )

        ( _, ReceiveNotification (Err err) ) ->
          ( { model | message = "[warning] invalid notification" }, Cmd.none )

        ( WaitingStart _, ReceiveNotification (Ok (NotifyGameStart ostate)) ) ->
          ( { model | state = InRoom user { pstate0 | game = PlayingGame ostate } chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyComment comment)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogComment comment ] } in
          ( { model | state = InRoom user pstate1 chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyEntered userIdEntered)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogEntered userIdEntered ] } in
          ( { model | state = InRoom user pstate1 chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyExited userIdExited)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogExited userIdExited ] } in
          ( { model | state = InRoom user pstate1 chatTextInput0 }, Cmd.none )

        _ ->
          ( { model | message = "[warning] unexpected message (InRoom): " ++ showMessage msg }, Cmd.none )


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
