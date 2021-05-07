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


initInputs : InputModel
initInputs =
  { userName = ""
  , roomName = ""
  , chatText = ""
  }


init : String -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flagString url navKey =
  let
    maybeUser : Maybe User
    maybeUser =
      case Flag.decode flagString of
          Ok(maybeUser0) -> maybeUser0
          Err(_)         -> Nothing

    ( cmd, state, preparation ) =
      case maybeUser of
        Nothing ->
          ( Cmd.none, AtEntrance, NoPreparation )

        Just user ->
          let cmd1 = WebSocketClient.setUserId user.id in
          case user.belongsTo of
            Nothing ->
              let cmd2 = Cmd.map Receive HttpClient.getAllRooms in
              ( Cmd.batch [ cmd1, cmd2 ]
              , AtEntrance
              , MovingToPlaza { user = Just user, rooms = Nothing }
              )

            Just roomId ->
              let cmd2 = Cmd.map Receive (HttpClient.getRoom roomId) in
              ( Cmd.batch [ cmd1, cmd2 ]
              , AtPlaza { user = user, rooms = [] }
              , EnteringRoom
              )

    model : Model
    model =
      { navigationKey = navKey
      , message       = "flags: " ++ flagString
      , inputs        = initInputs
      , state         = state
      , preparation   = preparation
      }
  in
  ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateInput inputUpdate ->
      case inputUpdate of
        UserNameInput userName ->
          let inputs = model.inputs in
          ( { model | inputs = { inputs | userName = userName } }, Cmd.none )

        RoomNameInput roomName ->
          let inputs = model.inputs in
          ( { model | inputs = { inputs | roomName = roomName } }, Cmd.none )

        ChatInput text ->
          let inputs = model.inputs in
          ( { model | inputs = { inputs | chatText = text } }, Cmd.none )

    Send req ->
      let ( model1, cmd ) = updateByHttpRequest req model in
      ( model1, Cmd.map Receive cmd )

    Receive response ->
      case response of
        AllRoomsGot result ->
          case model.preparation of
            MovingToPlaza r0 ->
              case result of
                Ok rooms ->
                  let r1 = { r0 | rooms = Just rooms } in
                  let
                    ( state1, preparation1 ) =
                      normalizePreparation ( model.state, MovingToPlaza r1 )
                  in
                  ( { model | state = state1, preparation = preparation1 }, Cmd.none )

                Err err ->
                  let
                    model1 =
                      { model
                      | message     = makeErrorMessage err
                      , preparation = NoPreparation
                      }
                  in
                  ( model1, Cmd.none )

            _ ->
              let
                model1 =
                  { model
                  | message     = "[warning] unexpected receiving of rooms"
                  , preparation = NoPreparation
                  }
              in
              ( model1, Cmd.none )

        UserCreated userName result ->
          case ( model.state, model.preparation ) of
            ( AtEntrance, MovingToPlaza r ) ->
              case result of
                Ok userId ->
                  let user = { id = userId, name = userName, belongsTo = Nothing } in
                  let cmd = WebSocketClient.setUserId userId in
                  let
                    ( state1, preparation1 ) =
                      normalizePreparation ( model.state, MovingToPlaza { r | user = Just user } )
                  in
                  ( { model | state = state1, preparation = preparation1 }, cmd )

                Err err ->
                  ( resetPreparationByError model err, Cmd.none )

            _ ->
              ( { model | message = "[warning] unexpected user creation" }, Cmd.none )

        RoomCreated roomName result ->
          case result of
            Ok roomId ->
              let room = { id = roomId, name = roomName, members = [] } in
              case model.state of
                  AtPlaza plaza ->
                    let
                      plaza1 = { plaza | rooms = room :: plaza.rooms }
                    in
                    ( { model | state = AtPlaza plaza1 }, Cmd.none )

                  _ ->
                    ( { model | message = "[warning] unexpected room creation" }, Cmd.none )

            Err err ->
              ( resetPreparationByError model err, Cmd.none )

        RoomEntered roomId result ->
          case ( model.state, model.preparation ) of
            ( AtPlaza r, EnteringRoom ) ->
              case result of
                Ok room ->
                  let
                    model1 =
                      { model
                      | state       = InRoom { user = r.user, room = room, roomState = () }
                      , preparation = NoPreparation
                      }
                  in
                  ( model1, Cmd.none )

                Err err ->
                  ( resetPreparationByError model err, Cmd.none )

            _ ->
              ( { model | message = "[warning] unexpected room entry" }, Cmd.none )

    SendWebSocketMessage wsreq ->
        updateByWebSocketRequest wsreq model

    ReceiveWebSocketMessage s ->
        ( {model | message = "[websocket] " ++ s }, Cmd.none )

    UrlChange url ->
      ( model, Cmd.none )

    UrlRequest urlRequest ->
      case urlRequest of
        Browser.External href ->
          ( model, Navigation.load href )

        Browser.Internal url ->
          ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )


normalizePreparation : ( State, Preparation ) -> ( State, Preparation )
normalizePreparation ( state, preparation ) =
  case preparation of
    MovingToPlaza r ->
      case ( r.user, r.rooms ) of
        ( Just user, Just rooms ) ->
          ( AtPlaza { user = user, rooms = rooms }, NoPreparation )

        _ ->
          ( state, preparation )

    _ ->
      ( state, preparation )


resetPreparationByError : Model -> Http.Error -> Model
resetPreparationByError model err =
  { model
  | message     = makeErrorMessage err
  , preparation = NoPreparation
  }


view : Model -> Browser.Document Msg
view model =
  { title = "tianjiupai"
  , body  = View.viewBody model
  }


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


updateByHttpRequest : Request -> Model -> ( Model, Cmd Response )
updateByHttpRequest req model =
  case req of
    CreateUser ->
      case model.state of
        AtEntrance ->
          let userName = model.inputs.userName in
          let cmd = HttpClient.createUser userName in
          ( model, cmd )

        _ ->
          ( model, Cmd.none )

    CreateRoom ->
      case model.state of
        AtPlaza r ->
          let inputs = model.inputs in
          let cmd = HttpClient.createRoom r.user.id inputs.roomName in
          ( { model | inputs = { inputs | roomName = "" } }, cmd )

        _ ->
          ( model, Cmd.none )

    EnterRoom roomId ->
      case model.state of
        AtPlaza r ->
          let cmd = HttpClient.enterRoom r.user.id roomId in
          ( model, cmd )

        _ ->
          ( model, Cmd.none )


updateByWebSocketRequest : WebSocketRequest -> Model -> ( Model, Cmd Msg )
updateByWebSocketRequest wsreq model =
  case wsreq of
    SendChat ->
      let cmd = WebSocketClient.sendChat model.inputs.chatText in
      let inputs = model.inputs in
      ( { model | inputs = { inputs | chatText = "" }}, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocketClient.subscribe
