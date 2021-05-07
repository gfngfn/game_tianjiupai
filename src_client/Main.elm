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
import WebSocketClient


host : String
host =
  "localhost:8080"


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
      }

    cmd : Cmd Msg
    cmd =
      case maybeUser of
          Nothing   -> Cmd.none
          Just user -> WebSocketClient.setUserId user.id
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

        ChatInput text ->
          let inputs = model.inputs in
          ( { model | inputs = { inputs | chatText = text } }, Cmd.none )

    Send req ->
      let cmd = makeCmdFromRequest req model in
      ( model, Cmd.map Receive cmd )

    Receive response ->
      case response of
        UserCreated userName result ->
          case result of
            Ok userId ->
              let user = { id = userId, name = userName } in
              let cmd = WebSocketClient.setUserId userId in
              ( { model | user = Just user }, cmd )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

    SendWebSocketMessage wsreq ->
        let cmd = makeCmdFromWebSocketRequest wsreq model in
        ( model, cmd )

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
          viewRoomList model user
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


viewRoomList : Model -> User -> Html Msg
viewRoomList model user =
  div []
    [ div []
        [ text ("Hi, " ++ user.name ++ "! (your user ID: " ++ user.id ++ ")") ]
    , div []
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


makeCmdFromRequest : Request -> Model -> Cmd Response
makeCmdFromRequest req model =
  case req of
    CreateUser ->
      let userName = model.inputs.userName in
      Http.post
        { url    = "http://" ++ host ++ "/users"
        , body   = Http.jsonBody (JE.object [ ( "user_name", JE.string userName ) ])
        , expect = Http.expectJson (UserCreated userName) (JD.field "user_id" JD.string)
        }


makeCmdFromWebSocketRequest : WebSocketRequest -> Model -> Cmd Msg
makeCmdFromWebSocketRequest wsreq model =
  case wsreq of
    SendChat ->
      WebSocketClient.sendChat model.inputs.chatText


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocketClient.subscribe
