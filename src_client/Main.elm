module Main exposing (..)

import Set exposing (Set)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)
import Url exposing (Url)
import Time
import Http
import Browser
import Browser.Events

import Common exposing (..)
import Models exposing (..)
import Constants
import HttpClient
import WebSocketClient
import PerSeat
import View


main =
  Browser.document
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }


type alias Flag =
  { user         : String
  , windowWidth  : Int
  , windowHeight : Int
  , httpOrigin   : Origin
  }


makeWebsocketOrigin : Origin -> Origin
makeWebsocketOrigin httpOrigin =
  if httpOrigin |> String.startsWith "http://" then
    "ws://" ++ String.dropLeft 7 httpOrigin
  else if httpOrigin |> String.startsWith "https://" then
    "ws://" ++ String.dropLeft 8 httpOrigin
  else
    "ws://this-cannot-happen"


init : Flag -> ( Model, Cmd Msg )
init flag =
  let
    wsOrigin : Origin
    wsOrigin = makeWebsocketOrigin flag.httpOrigin

    maybeFlagUser : Maybe FlagUser
    maybeFlagUser =
      case JD.decodeString (decodeOption decodeFlagUser) flag.user of
        Ok(maybeFlagUser0) -> maybeFlagUser0
        Err(err)           -> let _ = Debug.log "failed to parse" err in Nothing

    ( cmd, state ) =
      case maybeFlagUser of
        Nothing ->
          let _ = Debug.log "no user" () in
          ( Cmd.none, AtEntrance "" Nothing )

        Just flagUser ->
          let _ = Debug.log "flag user" flagUser in
          let userId = flagUser.id in
          let userName = flagUser.name in
          let cmd0 = WebSocketClient.listen wsOrigin userId in
          let
            user : User
            user =
              { userId = userId, userName = userName }
          in
          ( cmd0, AtEntrance "" (Just ( user, flagUser.belongsTo )) )

    model : Model
    model =
      { message = ( Information, "flag user: " ++ flag.user ++ ", window width: " ++ String.fromInt flag.windowWidth )
      , state   = state
      , window  = { width = flag.windowWidth, height = flag.windowHeight }
      , origin  = flag.httpOrigin
      }
  in
  ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.state of
    AtEntrance userNameInput maybeUserAndRoom ->
      case msg of
        WindowResized width height ->
          ( { model | window = { width = width, height = height } }, Cmd.none )

        Heartbeat ->
          ( model, Cmd.none )

        UpdateInput (UserNameInput userNameInput1) ->
          ( { model | state = AtEntrance userNameInput1 maybeUserAndRoom }, Cmd.none )

        SendRequest CreateUser ->
          let cmd = HttpClient.createUser model.origin userNameInput in
          ( model, cmd )

        ReceiveResponse (UserCreated userName result) ->
          case result of
            Ok responseBody ->
              let userId = responseBody.userId in
              let
                user : User
                user = { userId = userId, userName = userName }
              in
              let wsOrigin = makeWebsocketOrigin model.origin in
              let cmd = WebSocketClient.listen wsOrigin userId in
              ( { model | state = AtEntrance userNameInput (Just ( user, Nothing )) }, cmd )

            Err err ->
              ( { model | message = makeErrorMessage "user creation" err }, Cmd.none )

        OpenWebSocket ws ->
            case maybeUserAndRoom of
              Just ( user, Nothing ) ->
                let cmd = HttpClient.getAllRooms model.origin in
                ( { model | state = AtPlaza ws user "" Nothing }, cmd )

              Just ( user, Just roomId ) ->
                let cmd = HttpClient.getRoom model.origin user.userId roomId in
                ( { model | state = AtPlaza ws user "" Nothing }, cmd )

              Nothing ->
                let message = ( Warning, "unexpected message (AtEntrance): " ++ showMessage msg ) in
                ( { model | message = message }, Cmd.none )

        _ ->
          let message = ( Warning, "unexpected message (AtEntrance): " ++ showMessage msg ) in
          ( { model | message = message }, Cmd.none )

    AtPlaza ws user roomNameInput0 maybeRooms ->
      case msg of
        WindowResized width height ->
          ( { model | window = { width = width, height = height } }, Cmd.none )

        Heartbeat ->
          let cmd = WebSocketClient.sendHeartbeat ws in
          ( model, cmd )

        UpdateInput (RoomNameInput roomNameInput1) ->
           ( { model | state = AtPlaza ws user roomNameInput1 maybeRooms }, Cmd.none )

        ReceiveResponse (AllRoomsGot result) ->
          case result of
            Ok responseBody ->
              let rooms = responseBody.rooms in
              ( { model | state = AtPlaza ws user roomNameInput0 (Just rooms) }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage "rooms" err }, Cmd.none )

        SendRequest CreateRoom ->
          let cmd = HttpClient.createRoom model.origin user.userId roomNameInput0 in
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
                    roomSummaries1 = roomSummary :: roomSummaries0
                  in
                  ( { model | state = AtPlaza ws user roomNameInput0 (Just roomSummaries1) }, Cmd.none )

                Nothing ->
                  ( model, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage "room creation" err }, Cmd.none )

        SendRequest (EnterRoom roomId) ->
          let cmd = HttpClient.enterRoom model.origin user.userId roomId in
          ( model, cmd )

        ReceiveResponse (RoomEntered roomId result) ->
          case result of
            Ok pstate0 ->
              case pstate0.game of
                WaitingStart _ ->
                  ( { model | state = InRoom ws user pstate0 Set.empty "" }, Cmd.none )

                PlayingGame ostate0 ->
                  let cmd = WebSocketClient.sendAck ws ostate0.snapshotId in
                  let ostate1 = { ostate0 | synchronizing = True } in
                  let pstate1 = { pstate0 | game = PlayingGame ostate1 } in
                  Debug.log "RoomEntered (+)" ( { model | state = InRoom ws user pstate1 Set.empty "" }, cmd )

            Err err ->
              ( { model | message = makeErrorMessage "enter room" err }, Cmd.none )

        ReceiveResponse (RoomGot roomId result) ->
          case result of
            Ok pstate ->
              ( { model | state = InRoom ws user pstate Set.empty "" }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage "got room" err }, Cmd.none )

        _ ->
          ( { model | message = ( Warning, "unexpected message (AtPlaza): " ++ showMessage msg ) }, Cmd.none )

    InRoom ws user pstate0 indices0 chatTextInput0 ->
      case ( pstate0.game, msg ) of
        ( _, WindowResized width height ) ->
          ( { model
            | window  = { width = width, height = height }
            , message = ( Information, "window resized. width: " ++ String.fromInt width )
            }
          , Cmd.none
          )

        ( _, Heartbeat ) ->
          let cmd = WebSocketClient.sendHeartbeat ws in
          ( model, cmd )

        ( _, UpdateInput (ChatInput chatTextInput1) ) ->
           ( { model | state = InRoom ws user pstate0 indices0 chatTextInput1 }, Cmd.none )

        ( _, SendRequest SendChat ) ->
          let cmd = WebSocketClient.sendChat ws chatTextInput0 in
          ( { model | state = InRoom ws user pstate0 indices0 ""  }, cmd )

        ( _, SendRequest (ExitRoom roomId) ) ->
          let cmd = HttpClient.exitRoom model.origin user.userId roomId in
          ( model, cmd )

        ( _, ReceiveResponse (RoomExited roomId res) ) ->
          case res of
            Ok _ ->
              let cmd = HttpClient.getAllRooms model.origin in
              ( { model | state = AtPlaza ws user "" Nothing }, cmd )

            Err err ->
              ( { model | message = makeErrorMessage "room exit" err }, Cmd.none )

        ( _, ReceiveNotification (Err err) ) ->
          ( { model | message = ( Warning, "invalid notification: " ++ JD.errorToString err ) }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyComment comment)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogComment comment ] } in
          ( { model | state = InRoom ws user pstate1 indices0 chatTextInput0 }, Cmd.none )

        ( WaitingStart users0, ReceiveNotification (Ok (NotifyEntered userEntered)) ) ->
          let
            users1 =
              case users0 |> List.filter (\u -> u.userId == userEntered.userId) of
                []     -> users0 ++ [ userEntered ]
                _ :: _ -> Debug.log ("Warning: received NotifyEntered, but already contains " ++ userEntered.userId) users0
          in
          let pstate1 = { pstate0 | game = WaitingStart users1, logs = pstate0.logs ++ [ LogEntered userEntered ] } in
          ( { model | state = InRoom ws user pstate1 indices0 chatTextInput0 }, Cmd.none )

        ( game0, ReceiveNotification (Ok (NotifyExited userExited)) ) ->
          let
            game1 =
              case game0 of
                PlayingGame ostate0 ->
                  let meta0 = ostate0.meta in
                  let players0 = meta0.players in
                  case
                    PerSeat.find (\maybePlayer ->
                      case maybePlayer of
                        Nothing     -> False
                        Just player -> player.user.userId == userExited.userId
                    ) players0
                  of
                    Nothing ->
                      Debug.log ("Warning: received NotifyExited, but already no " ++ userExited.userId) game0

                    Just seat ->
                      let players1 = PerSeat.update seat Nothing players0 in
                      PlayingGame { ostate0 | meta = { meta0 | players = players1 } }

                WaitingStart users0 ->
                  WaitingStart (users0 |> List.filter (\u -> u.userId /= user.userId))
          in
          let pstate1 = { pstate0 | game = game1, logs = pstate0.logs ++ [ LogExited userExited ] } in
          ( { model | state = InRoom ws user pstate1 indices0 chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyConnection connection)) ) ->
          let pstate1 = { pstate0 | logs = pstate0.logs ++ [ LogConnection connection ] } in
          ( { model | state = InRoom ws user pstate1 indices0 chatTextInput0 }, Cmd.none )

        ( _, ReceiveNotification (Ok (NotifyGameStart ostate0)) ) ->
          let cmd = WebSocketClient.sendAck ws ostate0.snapshotId in
          let ostate1 = { ostate0 | synchronizing = True } in
          let meta = ostate1.meta in
          let gameIndex = { inningIndex = meta.inningIndex, numConsecutives = meta.numConsecutives } in
          let pstate1 = { pstate0 | game = PlayingGame ostate1, logs = pstate0.logs ++ [ LogGameStart gameIndex ] } in
          let state1 = InRoom ws user pstate1 indices0 chatTextInput0 in
          Debug.log "NotifyGameStart (+)" ( { model | state = state1 }, cmd )

        ( PlayingGame ostate0, ReceiveNotification (Ok (NotifyEnteredMidway midwayEnter)) ) ->
          let userEntered = midwayEnter.user in
          let seat = midwayEnter.seat in
          let meta0 = ostate0.meta in
          let players1 = PerSeat.update seat (Just { user = user, isConnected = True }) meta0.players in
          let ostate1 = { ostate0 | meta = { meta0 | players = players1 } } in
          let pstate1 = { pstate0 | game = PlayingGame ostate1, logs = pstate0.logs ++ [ LogEntered userEntered ] } in
          ( { model | state = InRoom ws user pstate1 indices0 chatTextInput0 }, Cmd.none )

        ( PlayingGame ostate0, ReceiveNotification (Ok (NotifySubmission submission)) ) ->
        -- When receiving a submission of another player:
          let newState = submission.newState in
          let
            maybeNext =
              if ostate0.synchronizing then
                Nothing
              else
                -- TODO: extract (possibly hidden) submitted cards and a submitter from `submission` for animation
                case ( submission.trickLast, ostate0.observableInning ) of
                  ( Just observableLast, ObservableDuringInning oinning0 ) ->
                  -- If this is the last submission within a trick:
                  -- begins to synchronize, and waits `Constants.trickLastTimeMs' milliseconds.
                    let
                      oinning1 =
                        { oinning0 | table = observableLast.table }

                      ostate1 =
                        { ostate0
                        | synchronizing    = True
                        , observableInning = ObservableDuringInning oinning1
                        }

                      logs1 =
                        case observableLast.diffs of
                          Nothing    -> pstate0.logs
                          Just diffs -> pstate0.logs ++ [ LogDiffs diffs ]

                      pstate1 =
                        { pstate0
                        | game = PlayingGame ostate1
                        , logs = logs1
                        }
                    in
                    let state1 = InRoom ws user pstate1 indices0 chatTextInput0 in
                    let cmd = sendAfter Constants.trickLastTimeMs (TransitionToNextTrick newState) in
                    Just ( { model | state = state1 }, cmd )

                  ( Just _, _ ) ->
                    Nothing

                  ( Nothing, _ ) ->
                  -- If this is NOT the last submission within a trick:
                  -- sends ACK, updates the state, and begins to synchronize.
                    let ostate1 = { newState | synchronizing = True } in
                    let state1 = InRoom ws user { pstate0 | game = PlayingGame ostate1 } indices0 chatTextInput0 in
                    let cmd = WebSocketClient.sendAck ws ostate1.snapshotId in
                    Just ( { model | state = state1 }, cmd )
          in
          case maybeNext of
            Just next ->
              Debug.log "NotifySubmission (+)" next

            Nothing ->
              let message = ( Warning, "unexpected message (InRoom): " ++ showMessage msg ) in
              ( { model | message = message }, Cmd.none )

        ( PlayingGame ostate0, ReceiveResponse (SubmissionDone res) ) ->
        -- When receiving a response of a submission performed by the current user:
          let
            nextResult =
              case res of
                Ok submissionResponse ->
                  let newState = submissionResponse.newState in
                  case ( submissionResponse.trickLast, ostate0.observableInning ) of
                    ( Just last, ObservableDuringInning oinning0 ) ->
                    -- If this is the last submission within a trick:
                    -- keeps synchronizing and waits `Constants.trickLastTimeMs' milliseconds.
                      let
                        oinning1 =
                          { oinning0
                          | table    = last.table
                          , yourHand = last.hand
                          }

                        ostate1 =
                          { ostate0
                          | observableInning = ObservableDuringInning oinning1
                          }

                        logs1 =
                          case last.diffs of
                            Nothing    -> pstate0.logs
                            Just diffs -> pstate0.logs ++ [ LogDiffs diffs ]

                        pstate1 =
                          { pstate0
                          | game = PlayingGame ostate1
                          , logs = logs1
                          }
                      in
                      let state1 = InRoom ws user pstate1 indices0 chatTextInput0 in
                      let cmd = sendAfter Constants.trickLastTimeMs (TransitionToNextTrick newState) in
                      let
                        message =
                          if ostate0.synchronizing then
                            model.message
                          else
                            ( Warning, "not synchronizing: " ++ showMessage msg )
                      in
                      Ok ( { model | message = message, state = state1 }, cmd )

                    ( Just _, _ ) ->
                      Err ( Warning, "unexpected message (Just, _): " ++ showMessage msg )

                    ( Nothing, _ ) ->
                    -- If this is NOT the last submission within a trick:
                    -- sends ACK, updates the state, and keeps synchronizing.
                      let ostate1 = newState in -- `synchronizing` is made `True`
                      let state1 = InRoom ws user { pstate0 | game = PlayingGame ostate1 } indices0 chatTextInput0 in
                      let cmd = WebSocketClient.sendAck ws ostate1.snapshotId in
                      let
                        message =
                          if ostate0.synchronizing then
                            model.message
                          else
                            ( Warning, "not synchronizing: " ++ showMessage msg )
                      in
                      Ok ( { model | message = message, state = state1 }, cmd )

                Err err ->
                  Err (makeErrorMessage "submission" err)
          in
          case nextResult of
            Ok next ->
              Debug.log "SubmissionDone" next

            Err message ->
              let ostate1 = { ostate0 | synchronizing = False } in
              ( { model
                | message = message
                , state   = InRoom ws user { pstate0 | game = PlayingGame ostate1 } indices0 chatTextInput0
                }
              , Cmd.none
              )

        ( PlayingGame ostate0, TransitionToNextTrick ostate1 ) ->
        -- Sends ACK, updates the state to the beginning of the next trick, and keeps synchronizing.
          if ostate0.synchronizing then
            let ostate2 = { ostate1 | synchronizing = True } in
            let state1 = InRoom ws user { pstate0 | game = PlayingGame ostate2 } indices0 chatTextInput0 in
            let cmd = WebSocketClient.sendAck ws ostate1.snapshotId in
            Debug.log "TransitionToNextTrick" ( { model | state = state1 }, cmd )
          else
            ( { model | message = ( Warning, "unexpected message (InRoom): " ++ showMessage msg ) }, Cmd.none )

        ( PlayingGame ostate0, ReceiveNotification (Ok NotifyNextStep) ) ->
        -- Ends synchronization.
          if ostate0.synchronizing then
            let ostate1 = { ostate0 | synchronizing = False } in
            let state1 = InRoom ws user { pstate0 | game = PlayingGame ostate1 } indices0 chatTextInput0 in
            Debug.log "NotifyNextStep (-)" ( { model | state = state1 }, Cmd.none )
          else
            ( { model | message = ( Warning, "unexpected message (InRoom): " ++ showMessage msg ) }, Cmd.none )

        ( PlayingGame ostate0, SendRequest SubmitCards ) ->
        -- Submits cards and begins to synchronize.
          case ostate0.observableInning of
            ObservableDuringInning inning ->
              let ostate1 = { ostate0 | synchronizing = True } in
              let indices1 = Set.empty in
              let state1 = InRoom ws user { pstate0 | game = PlayingGame ostate1 } indices1 chatTextInput0 in
              let cards = inning.yourHand |> pickupSelectedCards indices0 in
              let cmd = HttpClient.submitCards model.origin user.userId pstate0.room.roomId cards in
              Debug.log "SubmitCards (+)" ( { model | state = state1 }, cmd )

            ObservableInningEnd _ ->
              ( { model | message = ( Warning, "inning has already ended" ) }, Cmd.none )

        ( PlayingGame ostate0, SendRequest RequireNextInning ) ->
          let cmd = WebSocketClient.requireNextInning ws ostate0.snapshotId in
          let ostate1 = { ostate0 | synchronizing = True } in
          let state1 = InRoom ws user { pstate0 | game = PlayingGame ostate1 } indices0 chatTextInput0 in
          Debug.log "RequireNextInning (+)" ( { model | state = state1 }, cmd )

        ( PlayingGame _, SelectCard index ) ->
          let indices1 = indices0 |> Set.insert index in
          ( { model | state = InRoom ws user pstate0 indices1 chatTextInput0 }, Cmd.none )

        ( PlayingGame _, UnselectCard index ) ->
          let indices1 = indices0 |> Set.remove index in
          ( { model | state = InRoom ws user pstate0 indices1 chatTextInput0 }, Cmd.none )

        _ ->
          ( { model | message = ( Warning, "unexpected message (InRoom): " ++ showMessage msg ) }, Cmd.none )


pickupSelectedCards : Set Int -> List Card -> List Card
pickupSelectedCards indices cards =
  cards
    |> List.indexedMap (\index card -> ( index, card ))
    |> List.foldl (\( index, card ) cardAcc ->
      if indices |> Set.member index then
        card :: cardAcc
      else
        cardAcc
    ) []


view : Model -> Browser.Document Msg
view model =
  { title = "tianjiupai"
  , body  = View.viewBody model
  }


showNotification : Notification -> String
showNotification notification =
  case notification of
    NotifyComment _       -> "NotifyComment"
    NotifyEntered _       -> "NotifyEntered"
    NotifyExited _        -> "NotifyExited"
    NotifyGameStart _     -> "NotifyGameStart"
    NotifyNextStep        -> "NotifyNextStep"
    NotifySubmission _    -> "NotifySubmission"
    NotifyConnection _    -> "NotifyConnection"
    NotifyEnteredMidway _ -> "NotifyEnteredMidway"


showMessage : Msg -> String
showMessage msg =
  case msg of
    UpdateInput _               -> "UpdateInput"
    SendRequest _               -> "SendRequest"
    ReceiveResponse resp        -> "ReceiveResponse (" ++ showResponse resp ++ ")"
    ReceiveNotification (Err _) -> "ReceiveNotification (error)"
    ReceiveNotification (Ok nt) -> "ReceiveNotification (" ++ showNotification nt ++ ")"
    OpenWebSocket _             -> "OpenWebSocket"
    SelectCard _                -> "SelectCard"
    UnselectCard _              -> "UnselectCard"
    TransitionToNextTrick _     -> "TransitionToNextTrick"
    Heartbeat                   -> "Heartbeat"
    WindowResized _ _           -> "WindowResized"


showResponse : Response -> String
showResponse resp =
  case resp of
    SubmissionDone (Err err) -> "SubmissionDone (error: " ++ showHttpError err ++ ")"
    SubmissionDone (Ok _)    -> "SubmissionDone (ok)"
    _                        -> "<response>"


showHttpError : Http.Error -> String
showHttpError err =
  case err of
    Http.BadUrl s     -> "bad URL; " ++ s
    Http.Timeout      -> "request timeout"
    Http.BadBody s    -> "bad body; " ++ s
    Http.NetworkError -> "network error"
    Http.BadStatus n  -> "bad status; " ++ String.fromInt n


makeErrorMessage : String -> Http.Error -> ( MessageLevel, String )
makeErrorMessage category err =
  let msg = showHttpError err in
  ( Warning, "[error] " ++ category ++ ": " ++ msg )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
      [ WebSocketClient.onOpen
      , WebSocketClient.subscribe
      , Time.every Constants.heartbeatIntervalMs (\_ -> Heartbeat)
      , Browser.Events.onResize WindowResized
      ]
