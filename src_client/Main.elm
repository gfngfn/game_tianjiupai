module Main exposing (..)

import Json.Encode as JE
import Json.Decode as JD
import Url exposing (Url)
import Http
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Flags = String

type alias UserId = String

type alias Model =
  { navigationKey : Navigation.Key
  , message       : String
  , userName      : String
  , userId        : Maybe UserId
  }

type Request
  = CreateUser

type Response
  = UserCreated (Result Http.Error String)

type Msg
  = UrlChange Url
  | UrlRequest UrlRequest
  | UpdateUserName String
  | Send Request
  | Receive Response


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


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
  let
    model : Model
    model =
      { navigationKey = navKey
      , message       = "flags: " ++ flags
      , userName      = ""
      , userId        = Nothing
      }
  in
  ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateUserName userName ->
      ( { model | userName = userName }, Cmd.none )

    Send req ->
      let cmd = makeCmdFromRequest req model in
      ( model, Cmd.map Receive cmd )

    Receive response ->
      case response of
        UserCreated result ->
          case result of
            Ok userId ->
              ( { model | userId = Just userId }, Cmd.none )

            Err err ->
              ( { model | message = makeErrorMessage err }, Cmd.none )

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
      case model.userId of
        Nothing ->
          div []
            [ input
                [ type_ "text"
                , placeholder "username"
                , value model.userName
                , onInput UpdateUserName
                ] []
            , button
                [ onClick (Send CreateUser) ]
                [ text "start" ]
            ]

        Just userId ->
          div []
            [ text ("Hi, " ++ model.userName ++ "! (your user ID: " ++ userId ++ ")") ]
  in
  [ div []
      [ div [] [ text model.message ]
      , elemMain
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
      let userName = model.userName in
      Http.post
        { url    = "http://" ++ host ++ "/users"
        , body   = Http.jsonBody (JE.object [ ( "user_name", JE.string userName ) ])
        , expect = Http.expectJson UserCreated (JD.field "user_id" JD.string)
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
