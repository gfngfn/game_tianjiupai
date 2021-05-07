module Main exposing (..)

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)
import Url exposing (Url)
import Http
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Common exposing (..)
import Flag
import HttpClient
import WebSocketClient


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

    model : Model
    model =
      { navigationKey = navKey
      , message       = "flags: " ++ flagString
      , inputs        = initInputs
      , user          = maybeUser
      , rooms         = Nothing
      }

    cmd1 : Cmd Msg
    cmd1 =
      case maybeUser of
          Nothing   -> Cmd.none
          Just user -> WebSocketClient.setUserId user.id

    cmd2 : Cmd Msg
    cmd2 = Cmd.map Receive HttpClient.getRooms
  in
  ( model, Cmd.batch [cmd1, cmd2] )


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
        RoomsGot result ->
          case result of
            Ok rooms ->
              ( { model | rooms = Just rooms }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        UserCreated userName result ->
          case result of
            Ok userId ->
              let user = { id = userId, name = userName, belongsTo = Nothing } in
              let cmd = WebSocketClient.setUserId userId in
              ( { model | user = Just user }, cmd )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        RoomCreated roomName result ->
          case result of
            Ok roomId ->
              let room = { id = roomId, name = roomName, members = [] } in
              case model.rooms of
                  Nothing ->
                    ( model, Cmd.none )

                  Just rooms ->
                    ( { model | rooms = Just (room :: rooms) }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

        RoomEntered roomId result ->
          case result of
            Ok () ->
              case model.user of
                Nothing ->
                  ( model, Cmd.none )

                Just user ->
                  let
                    rooms =
                      case model.rooms of
                        Nothing ->
                          Nothing

                        Just rooms0 ->
                          let
                            rooms1 =
                              rooms0 |> List.map (\room ->
                                if room.id == roomId then
                                  if List.member user.id room.members then
                                    room
                                  else
                                    { room | members = room.members ++ [user.id] }
                                else
                                  room
                              )
                          in
                          Just rooms1
                  in
                  ( { model | rooms = rooms, user = Just { user | belongsTo = Just roomId } }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

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


view : Model -> Browser.Document Msg
view model =
  { title = "tianjiupai"
  , body  = viewBody model
  }


viewBody : Model -> List (Html Msg)
viewBody model =
  let
    elemMain =
      case model.user of
        Nothing ->
          viewEntrance model

        Just user ->
          case user.belongsTo of
            Nothing ->
              viewRoomListPage model user

            Just roomId ->
              viewRoomPage model roomId
  in
  [ div []
      [ div [] [ text model.message ]
      , elemMain
      ]
  ]


viewEntrance : Model -> Html Msg
viewEntrance model =
  div []
    [ input
        [ type_ "text"
        , placeholder "username"
        , value model.inputs.userName
        , onInput (UpdateInput << UserNameInput)
        ] []
    , button
        [ onClick (Send CreateUser) ]
        [ text "start" ]
    ]


viewRoomListPage : Model -> User -> Html Msg
viewRoomListPage model user =
  div []
    [ div []
        [ text ("Hi, " ++ user.name ++ "! (your user ID: " ++ user.id ++ ")") ]
    , viewRoomList model
    , div []
        [ input
            [ type_ "text"
            , placeholder "new room name"
            , value model.inputs.roomName
            , onInput (UpdateInput << RoomNameInput)
            ] []
        , button
            [ onClick (Send CreateRoom) ]
            [ text "create" ]
        ]
    ]


viewRoomList : Model -> Html Msg
viewRoomList model =
  case model.rooms of
    Nothing ->
      div [] [ text "(Rooms will be shown here)" ]

    Just rooms ->
      let
        elems =
          List.map (\room ->
            let members = String.join ", " room.members in
            li []
            [ text (room.name ++ " (room ID: " ++ room.id ++ ", members: " ++ members ++ ")")
            , button
                [ onClick (Send (EnterRoom room.id)) ]
                [ text "enter" ]
            ]
          ) rooms
      in
      ul [] elems


viewRoomPage : Model -> RoomId -> Html Msg
viewRoomPage model roomId =
  let
    members =
      case model.rooms of
        Nothing ->
          "(Cannot find the room. Please reload the page)"

        Just rooms ->
          case List.filter (\room -> room.id == roomId) rooms of
            []        -> "(Cannot find the room. Please reload the page)"
            room :: _ -> String.join ", " room.members
  in
  div []
    [ div []
        [ div []
            [ text ("Room (ID: " ++ roomId ++ ", members: " ++ members ++ ")") ]
        ]
    , div []
        [ div []
            [ input
                [ type_ "text"
                , placeholder "comment"
                , value model.inputs.chatText
                , onInput (UpdateInput << ChatInput)
                ] []
            , button
                [ onClick (SendWebSocketMessage SendChat) ]
                [ text "send" ]
            ]
        ]
    ]



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
      let userName = model.inputs.userName in
      let cmd = HttpClient.createUser userName in
      ( model, cmd )

    CreateRoom ->
      case model.user of
        Nothing ->
          ( model, Cmd.none )

        Just user ->
          let inputs = model.inputs in
          let cmd = HttpClient.createRoom user.id inputs.roomName in
          ( { model | inputs = { inputs | roomName = "" } }, cmd )

    EnterRoom roomId ->
      case model.user of
        Nothing ->
          ( model, Cmd.none )

        Just user ->
          let cmd = HttpClient.enterRoom user.id roomId in
          ( model, cmd )


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
